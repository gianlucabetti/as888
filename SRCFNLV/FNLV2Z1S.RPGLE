000100170126     /*PRM  tgtrls(V5R4M0)
000200111005     /*END
000300060517      *------------*
000400111005      *? FNLV2Z1S ?*--------------------------------------------------*
000500060518      *------------*                                                  *
000600111005      *?STAMPA SEGNACOLLO CON BARCODE A 18 CON ZEBRA 12 DOT/MM       ?*
000700060518      *?       CON BARCODE 3X70                                      ?*
000800060518      *---------------------------------------------------------------*
000900060523      *
001000060511     h decedit('0,') datedit(*dmy.)
001100060511      *
001200060516      *?ARCHIVI?------------------------------------------------------*
001300060511      *
001400060511     fFNLV22P   o    f  132        printer
001500060511      *
001600060516      *?COSTANTI?-----------------------------------------------------*
001700060511      *
001800060523      * - Caratteri numerici
001900060523     d C_DigitN        c                   const('0123456789')
002000060512      * - Costanti DPD
002100060519     d C_dpd00a        c                   const('Notification on dama-
002200060519     d                                     ge which is not ')
002300060519     d C_dpd00b        c                   const('recognizable from th-
002400060519     d                                     e outside has to')
002500060519     d C_dpd00c        c                   const('be submitted to DPD -
002600060519     d                                     within 7 days in')
002700060519     d C_dpd00d        c                   const('written.            -
002800060519     d                                                     ')
002900060517     d C_dpd01         c                   const('Delivery Adress:')
003000060512     d C_dpd03         c                   const('Sender:')
003100060512     d C_dpd04         c                   const('Receiver:')
003200060512     d C_dpd05         c                   const('Italy')
003300060512     d C_dpd09         c                   const('01')
003400111010     d C_dpd10         c                   const('BRT S.p.A')
003500060512     d C_dpd11         c                   const('sending')
003600060512     d C_dpd12         c                   const('consists of:')
003700060512     d C_dpd13         c                   const('parcel no:')
003800060512     d C_dpd14         c                   const('1 Parcel')
003900060512     d C_dpd15         c                   const('order no:')
004000060512     d C_dpd16         c                   const('date:')
004100060512     d C_dpd17         c                   const('weight in kg:')
004200060512     d C_dpd18         c                   const('delivery no:')
004300060512      * - Nazione di Partenza
004400060512     d C_nazP          c                   const('IT')
004500060512      *
004600060512      * - Caratteri "finali" in stampa - per segnacollo
004700060516     d C_End           c                   const('^FS')
004800060511      *
004900060516      *?SCHIERE?------------------------------------------------------*
005000060511      *
005100060519      * - Composizioni: Rag.Soc.Mitt.+Prov. Mitt./Dest.
005200060519      *                 Ind.+Loc.           Dest.
005300060512     d §RI             s              1    dim(37)  inz
005400060519      * - Sequenza colli: editati e separati da "-"
005500060512     d §RX             s              1    dim(11)  inz
005600060511      *
005700060516      *?STRUTTURE DATI?-----------------------------------------------*
005800060511      *
005900060512      * - Parametri
006000060511     d dsBLD         e ds
006100060511     d dsBLDE        e ds
006200060512      *
006300060512      * DS x composizione segnacollo lungo 7
006400060512      *    e con codice a barre (senza check-digit)
006500060512     d dsBLD7        e ds                           inz
006600060512     d  $SegnaC                1     17  0 dim(17)  inz
006700060511      *
006800060517      * Schiere per calcolo del check-digit
006900060512     d ds_BarCode      ds            18             inz
007000060512     d  $CB                    1     18  0 dim(18)  inz
007100060517     d ds2a            ds             2             inz
007200060512     d  §2N                    1      2  0 dim(2)   inz
007300060511      *
007400060517      * Campi per editazione e trattamento dei numeri segnacollo
007500060512     d ds7a            ds             7    inz
007600060512     d  §N7                    1      7  0 inz
007700060518     d  §s4                    1      4
007800060518     d  §s3                    5      7
007900060511      *
008000060518      * x DPD (DP3): Numero spedizione in etichetta Estero
008100060518     d DP3NSPds        ds                  inz
008200060518     d  DP3nsp_3a                     3    inz
008300060518     d  DP3nsp_8a                     8    inz
008400060518     d  DP3nsp_1a                     1    inz
008500060511      *
008600060518     *** * x EuroExpress: num. AWB
008700060518      * x DPD (DP3) .: "Dove"
008800060518     d DP3AWBds        ds                  inz
008900060518     d  wDPDc1w                       2    inz
009000060518     d  wDPDc3w                       5    inz
009100060518     d  wDPDc5w                       2    inz
009200060518     d  wDPDc7w                       1    inz
009300060511      * x DPD .......: "Dove" in etichetta Estero
009400060518     d DP3DOVEds       ds                  inz
009500060518     d  wDPDc1                        2    inz
009600060518     d  wDPDc2                        1    inz('-')
009700060518     d  wDPDc3                        5    inz
009800060518     d  wDPDc4                        1    inz('-')
009900060518     d  wDPDc5                        2    inz
010000060518     d  wDPDc6                        1    inz('-')
010100060518     d  wDPDc7                        1    inz
010200060511      *
010300060518      * x DPD (DP3): dati in etichetta Estero
010400060518     d DP3XX1ds        ds                  inz
010500060511      * - versione cappario
010600060518     d  wDP3xx1vcap                   5    inz
010700060511      * - *blanks
010800060518     d  wDP3xx1_                      5    inz
010900060511      * - linea di arrivo
011000060518     d  wDP3xx1lna                    3    inz
011100060511      * - zona di arrivo
011200060518     d  wDP3xx1zna                    2    inz
011300060518      *
011400060518      * x DPD (DP4): dati in etichetta Estero
011500060518     d DP4XX1ds        ds                  inz
011600060518      * - versione cappario
011700060518     d  wDP4xx1vcap                   7    inz
011800060518      * - depot
011900060518     d  wDP4xx1depo                   4    inz
012000060518      * - destination sort
012100060518     d  wDP4xx1dsor                   4    inz
012200060511      *
012300060511      * x DPD: numero di telefono
012400060512     d dsDPD2          ds                  inz
012500060511     d  DPDtel                        3    inz('Tel')
012600060512     d  DPDteln                      15    inz
012700060511      *
012800060518      * x DPD (DP3): numero di fax
012900060512     d dsDPD2f         ds                  inz
013000060511     d  DPDfax                        3    inz('Fax')
013100060512     d  DPDfaxn                      15    inz
013200060511      *
013300060511      * x DPD: testo "fisso" in etichetta
013400060518     d dsDPDn          ds                  inz
013500111007     d  wDPDn12                      76    inz
013600111007     d  wDPDn34                      76    inz
013700060518      *
013800060518      * x DPD (DP4): barcode (28 char + check-digit)
013900060523     d dsDP4_BarCode   ds            29    inz
014000060529      * - Identification tag ("%")                - alfanumerico
014100060518     d  DP4bc_IdTag                   1    inz
014200060529      * - Destination post code (c.a.p. - con "00" iniziale)
014300060518     d  DP4bc_DPC                     7    inz
014400060529      * - Tracking number (parcel)                - alfanumerico
014500060518     d  DP4bc_TrackNr                14    inz
014600060529      * - Service code                            - numerico
014700060606     d  DP4bc_SrvCd                   3  0 inz
014800060529      * - Destination country code (cod.ISO num.) - numerico
014900060606     d  DP4bc_DCC                     3  0 inz
015000060518      * - Check digit (sui caratteri escluso "IdTag")
015100060518     d  DP4bc_ChkD                    1    inz
015200060627     *** * - "spezzamento" tracking number per isolamento campi numerici
015300060627     ***d   DP4bc_TrackA          9     12
015400060627     ***d   DP4bc_TrackN         13     22  0
015500060529      * - "isolamento" campi per la stampa di "decodifica"
015600060523     d   DP4bc_01              2      5
015700060523     d   DP4bc_02              6      8
015800060523     d   DP4bc_03              9     12
015900060523     d   DP4bc_04             13     16
016000060523     d   DP4bc_05             17     20
016100060523     d   DP4bc_06             21     22
016200060523     d   DP4bc_07             23     25
016300060523     d   DP4bc_08             26     28
016400060518      *
016500060518      * x DPD (DP4): parcel/tracking number
016600060518     d dsDP4_Parcel    ds                  inz
016700060518     d  DP4pn_a                       4    inz
016800060518     d  DP4pn_b                       4    inz
016900060518     d  DP4pn_c                       4    inz
017000060518     d  DP4pn_d                       2    inz
017100060518      * - Check digit
017200060518     d  DP4pn_ChkD                    1    inz
017300060511      *
017400060512     d dsPIOPPA        ds                  inz
017500010719     d  operat                        1    inz
017600010719     d  pista                         1    inz
017700010719     d  pallet                        2    inz
017800060512      *
017900060512      * Data 8 cifre (per routine di inversione)
018000060518     d WdataAMG        ds             8    inz
018100060518     d  Waa                           4  0 inz
018200060518     d  Wmm                           2  0 inz
018300060518     d  Wgg                           2  0 inz
018400040614      *
018500060516      *?VARIABILI?----------------------------------------------------*
018600060511      *
018700060512      * - Indici di schiera
018800060512     d XY              s              3  0 inz
018900060512     d XZ              s              3  0 inz
019000060512      * - Data Spedizione in formato gg/mm/aaaa
019100060512     d DataSp          s              8  0 inz
019200060512      * - flags in stampa ("ristampa", "valore", ...)
019300060512     d WLDrst          s              1    inz
019400060512     d WLDrst2         s              1    inz
019500060512      * - numero serie + "-"
019600060512     d WLDnrt          s              3    inz('  -')
019700060512      * - "Rif.:" + codice prodotto
019800060512     d WRif21          s             21    inz
019900060512      * - Terminal di Arrivo (alfanum.)
020000060518     d WLDtnaO         s              3    inz
020100060512      * - P.O. di Arrivo     (alfanum.)
020200060518     d WLDlnaO         s              3    inz
020300060512      * - Zona segnacolli    (alfanum.)
020400060518     d WLDzscO         s              2    inz
020500060512      *
020600060512      * - Località Destinatario x DPD
020700060512     d WLDlod          s             25    inz
020800060517      * - Ragione Sociale Mittente x DPD
020900111007     d WLDrsm          s             16    inz
021000111006     d WLDrsm1         s             20    inz
021100111006     d WLDrsm2         s             15    inz
021200060607      * - Nazione del Deposito DPD (DP4)
021300060607     d WDEsia          s              2    inz
021400060518      * - Decodifica Service code x DPD (DP4)
021500060518     d WDPDsrvc        s             16    inz
021600061010      * - "-" + IATA code + Priority group DPD (DP4)
021700061010     d WDP4shpp        s              5    inz
021800060512      *
021900060512      * - Codice a Barre
022000060512     d WCDbar          s             18    inz
022100060512      * - Parte finale in stampa Codice a Barre
022200060512     d WBCend          s             20    inz
022300060523      * - Campi per l'impostazione dei dati da stampare nel barcode
022400060523     d wLast           s              1    inz
022500060523     d wChar           s              1    inz
022600060512      * - Campi di comodo per calcolo Check-Digit
022700060512     d wCHKDGT         s              1  0 inz
022800060512     d §TT             s              3  0 inz
022900060512     d §TT2            s              3  0 inz
023000060512     d §CTR            s              3  0 inz
023100060512     d §RST            s              3  0 inz
023200060512      *
023300060512      * - Campi di comodo
023400060512     d W0007a          s              7    inz
023500060512     d W0003a          s              3    inz
023600060512     d W0002a          s              2    inz
023700060511
023800060511      *---------------------------------------------------------------*
023900060516      *?INDICATORI UTILIZZATI                                        ?*
024000060512      *---------------------------------------------------------------*
024100060512      *  19 - Stampa Serie Segnacollo - se c'è ("..-")                *
024200060512      *  55 - Segnacollo estero EuroExpress                           *
024300060626      *  61 - Sigla Servizio <> *blanks (box da stampare)             *
024400060512      *---------------------------------------------------------------*
024500060511
024600060511     c     *Entry        plist
024700060511     c                   parm                    dsBLD
024800060511     c                   parm                    dsBLDE
024900060512      *
025000060512      * Inversione Data Spedizione (in fmt gg/mm/aaaa)
025100060518     c                   movel     §LDaas        WdataAMG
025200060518     c                   move      §LDmgs        WdataAMG
025300060518     c                   eval      DataSp  =  (Wgg * 1000000)
025400060518     c                                     +  (Wmm * 10000)
025500060518     c                                     +  Waa
025600050415      *
025700060512      * Impostazione Flag Ristampa: " ", "R", "M", "V", "X", "W"
025800060512     c*** già così:      clear                   wLDrst
025900060512     c*** già così:      clear                   wLDrst2
026000060512sel 1c                   SELECT
026100050415      *
026200060518w   1c                   WHEN      §LDrst  =  *blanks
026300050415      *
026400060518w   1c                   WHEN      §LDrst  =  'V'
026500060518     c                   eval      wLDrst2 =  §LDrst
026600050415      *
026700060518w   1c                   WHEN      §LDrst  =  'X'
026800060518     c                   eval      wLDrst  =  'R'
026900060518     c                   eval      wLDrst2 =  'V'
027000050415      *
027100060518w   1c                   WHEN      §LDrst  =  'W'
027200060518     c                   eval      wLDrst  =  'M'
027300060518     c                   eval      wLDrst2 =  'V'
027400050415      *
027500060512x   1c                   OTHER
027600060518     c                   eval      wLDrst  =  §LDrst
027700050415      *
027800060512e   1c                   ENDSL
027900050415      *
028000060512      * Impostazione nr. serie ("nn-")
028100060512     c                   movel     §LDnrs        wLDnrt
028200060512      *
028300060512      * Sostituzione "0" (anche non significativi) del segnacollo
028400060512      *          con "O"
028500060512      * SE NON passa da sr. di Stampa ETICHETTA ITALIA
028600060512if  1c                   IF        §DEtst  =  'EST'
028700060512     c                   exsr      sr_COLZER
028800060512e   1c                   ENDIF
028900060512      *
029000060512      * Stampa ETICHETTA ESTERO
029100060512if  1c                   IF             §DEtst  =  'EST'
029200060512     c                             or   §DEtst  =  'ALL'
029300060512      *
029400060512sel 2c                   SELECT
029500060512      * - EuroExpress
029600060517      *   (gestita in sr. sr_PRINT - etichetta Italia)
029700060517w   2c                   WHEN      §DEtse  =  'EEX'
029800060517      * - DPD (Deutscher Paket Dienst)
029900060530w   2c                   WHEN      §DEtse  =  'DP3'
030000060530     c                   exsr      sr_DPDdove
030100060530     c                   exsr      sr_DPDchi
030200060530     c                   except    LV22dpd3
030300060530w   2c                   WHEN      §DEtse  =  'DP4'
030400060518     c                   exsr      sr_DP4
030500060517     c                   except    LV22dpd4
030600060512e   2c                   ENDSL
030700060512      *
030800060512e   1c                   ENDIF
030900060512      *
031000060512      * Stampa ETICHETTA ITALIA
031100060512if  1c                   IF             §DEtst  =  'ITA'
031200060512     c                             or   §DEtst  =  'ALL'
031300060512     c                   exsr      sr_PRINT
031400060517e   1c                   ENDIF
031500060512      *
031600060512     c                   eval      *inLR   =  *on
031700060512
031800060512      *---------------------------------------------------------------*
031900060516      *?Impostazione & Stampa ETICHETTA ITALIA                       ?*
032000060512      *---------------------------------------------------------------*
032100060512     c     sr_PRINT      BEGSR
032200060517      *
032300060517      *?Impostazione indicatori? - - - - - - - - - - - - - - - - - - -*
032400060517      *
032500060517      * - *in19 = Stampa Serie Segnacollo - se c'è ("..-")
032600060517     c                   eval      *in19   =  (§LDnrs > *zeros)
032700060517      *
032800060517      * - *in55 = Segnacollo Estero EuroExpress
032900060517     c                   eval      *in55   =  (§DEtse = 'EEX')
033000060512      *
033100060517      *?Impostazione campi in stampa?- - - - - - - - - - - - - - - - -*
033200060512      *
033300060512      * Copia dati da dsBLD a dsBLD7
033400060512     c                   eval      §LClnp  =  §LDlnp
033500060512     c                   eval      §LClna  =  §LDlna
033600060512     c                   eval      §LCnrs  =  §LDnrs
033700060512     c                   eval      §LCncd  =  §LDncd
033800060512     c                   eval      §LCzsc  =  §LDzsc
033900060512      *** non cancellare MOVEL     §LDdla        dsPIOPPA
034000060512      *
034100060512      * Calcola cifra di controllo (check-digit) x modulo 10 IBM
034200060512     c                   exsr      sr_M10
034300060512     c                   movel     ds_BarCode    wCDbar
034400060512      *
034500060512      * Pulizia del flag "Tipo Servizio Bolle" SE:
034600060512     c                   if             §LDtsp  =  'D'
034700060512     c                             and  §LDlna  <  300
034800060512     c                   clear                   §LDtsp
034900060512     c                   endif
035000060512      *
035100060512      *
035200060512     c                   clear                   §RX
035300060512      * Toglie *zeros non significativi dal 1° numero colli
035400060512     c                   clear                   w0007a
035500060512     c                   eval      %subst( w0007a
035600060512     c                                   : %len(w0007a)-%len(§LDnc1)+1
035700060512     c                                   : %len(§LDnc1) ) =
035800060512     c                             %editc( §LDnc1 : 'Z' )
035900060512     c                   move      w0007a        w0003a
036000060512     c                   movea     w0003a        §RX(1)
036100060512     c                   eval      §RX(4)  =  '-'
036200060512      * Toglie *zeros non significativi dall'ultimo numero colli
036300060512     c                   clear                   w0007a
036400060512     c                   eval      %subst( w0007a
036500060512     c                                   : %len(w0007a)-%len(§LDnc2)+1
036600060512     c                                   : %len(§LDnc2) ) =
036700060512     c                             %editc( §LDnc2 : 'Z' )
036800060512     c                   move      w0007a        w0003a
036900060512     c                   movea     w0003a        §RX(5)
037000060515      * Aggiunge il codice autotrasportatore
037100060515     c                   move      §LDcpd        w0003a
037200060512     c                   movea     w0003a        §RX(9)
037300060512      *
037400060512      * Sostituisce "0" (anche non significativi) con "O":
037500060512      * - Segnacollo
037600060512     c                   exsr      sr_COLZER
037700060512      * - Terminal di Arrivo
037800060512     c                   move      §LDtna        w0003a
037900060512     c     '0':'O'       xlate     w0003a        wLDtnaO
038000060512      * - P.O. di Arrivo
038100060512     c                   move      §LDlna        w0003a
038200060512     c     '0':'O'       xlate     w0003a        wLDlnaO
038300060512      * - Zona Segnacollo
038400060512     c                   movel     §LDzsc        w0002a
038500060512     c     '0':'O'       xlate     w0002a        wLDzscO
038600060512      *
038700060512      * Codice prodotto
038800060512     c                   clear                   wRif21
038900060512if  1c                   if        §LDcdp  <> *blanks
039000060512     c                   eval      wRif21  =  'Rif.: '
039100060512     c                                     +  §LDcdp
039200060512e   1c                   endif
039300060512      *
039400060517      *?Stampa?- - - - - - - - - - - - - - - - - - - - - - - - - - - -*
039500060512      *
039600060512      * Inizializzazione stampante
039700060512     c                   EXCEPT    LV22
039800060512      *
039900060512      * Stampa LINEA PARTENZA
040000060512     c                   EXCEPT    LV22H
040100060512      *
040200060512      * Stampa MITTENTE
040300060512     c                   clear                   §RI
040400060512     c                   movea     §LDrsm        §RI(4)
040500060512     c                   movea     *blanks       §RI(30)
040600060512     c                   movea     §LDprm        §RI(31)
040700060512     c                   clear                   §RI(33)
040800060512     c                   EXCEPT    LV22D1
040900060512      *
041000060512      * Stampa NUMERO SEGNACOLLO
041100060512     c                   EXCEPT    LV22D2
041200060512      *
041300060512      * Stampa LINEA ARRIVO
041400060512     c                   EXCEPT    LV22D3
041500060512      *
041600060512      * Stampa DESTINATARIO (1: ragione sociale e provincia)
041700060512     c                   clear                   §RI
041800060512     c                   movea     §LDrsd        §RI(4)
041900060512     c                   movea     *blanks       §RI(30)
042000060512     c                   movea     §LDprd        §RI(31)
042100060512     c                   clear                   §RI(33)
042200060512     c                   EXCEPT    LV22D4
042300060512      *
042400060512      * Stampa DESTINATARIO (2: indirizzo e località)
042500060512     c                   clear                   §RI
042600060512     c                   movea     §LDind        §RI(4)
042700060512      * - cerca l'ultimo carattere utilizzato dall'indirizzo in §RI
042800060512      *   (dove parte dal 4°, cioè ha 3 *blanks iniziali)
042900060512     c                   eval      XY  =  %len(%trimr(§LDind))
043000060512     c                   eval      XY  =  XY + 3
043100060512     c                   if        XY  >  25
043200060512     c                   movea     *blanks       §RI(26)
043300060512     c                   eval      XY  =  25
043400060512     c                   endif
043500060512      * - aggiunge la località del destinatario
043600060512     c                   add       3             XY
043700060512     c                   movea     §LDlod        §RI(XY)
043800060512     c                   EXCEPT    LV22D5
043900060512      *
044000060512      * Stampa TIPO SERVIZIO BOLLE (se impostato)
044100060512sel 1c                   select
044200060518w   1c                   when      §LDtsp  =  *blanks
044300090806w   1c                   when      §LDtsp  =  'E'   or
044400090806     c                             §LDtsp  =  'H'
044500060512     c                   EXCEPT    LV22X5E
044600060512x   2c                   other
044700060512     c                   EXCEPT    LV22X5
044800060512e   1c                   endsl
044900060512      *
045000060512      * Stampa BARCODE
045100060512     c                   EXCEPT    LV22D6
045200060512      *
045300060512      * Stampa RIFERIMENTO / RISTAMPA
045400060512     c                   EXCEPT    LV22D7
045500060512      *
045600060512      * "Chiusura" etichetta
045700060512     c                   EXCEPT    LV22D8
045800060512      *
045900060512     c                   ENDSR
046000060512
046100060512      *---------------------------------------------------------------*
046200060516      *?Sostituzione degli zeri NON significativi del segnacollo     ?*
046300060516      *?               con "O" (più leggibile)                       ?*
046400060512      *---------------------------------------------------------------*
046500060512     c     sr_COLZER     BEGSR
046600060511      *
046700060512     c                   move      §LDncd        §N7
046800060511     c     '0':'O'       xlate     ds7a          ds7a
046900060511      *
047000060512     c                   ENDSR
047100060512
047200060512      *---------------------------------------------------------------*
047300060516      *?Calcolo cifra di controllo secondo "modulo 10 IBM"           ?*
047400060516      *?"CHIODI": codice (di cui calcolare il check digit) lungo 17  ?*
047500060516      *?¯¯¯¯¯¯¯¯  check-digit alla 18ª posizione dello stesso campo  ?*
047600060512      *---------------------------------------------------------------*
047700060512     c     sr_M10        BEGSR
047800060512      *
047900060512     c                   clear                   §TT
048000060512     c                   clear                   §TT2
048100060512     c                   clear                   §CTR
048200060512     c                   clear                   §RST
048300060512     c                   clear                   wCHKDGT
048400060518     c                   eval      XY      =  18
048500060512      *
048600060512do  1c                   DO        17            XZ
048700060512      *
048800060512     c                   sub       1             XY
048900060512     c     XZ            div       2             §CTR
049000060512     c                   mvr                     §RST
049100060512      *
049200060518if  2c                   if        §RST    >  0
049300060512      *
049400060518     c                   eval      §CTR    =  $SegnaC(XY) * 2
049500060517     c                   move      §CTR          ds2a
049600060512     c                   xfoot     §2N           §CTR
049700060512      *
049800060512x   2c                   else
049900060512      *
050000060518     c                   eval      §CTR    =  $SegnaC(XY)
050100060512      *
050200060512e   2c                   endif
050300060512      *
050400060512     c                   add       §CTR          §TT
050500060512      *
050600060512e   1c                   ENDDO
050700060512      *
050800060512     c                   eval      §TT2    =  §TT  + 9
050900060512     c                   move      0             §TT2
051000060512     c                   eval      wCHKDGT = §TT2 - §TT
051100060512      *
051200060512     c                   movea     $SegnaC       $CB
051300060512     c                   eval      $CB(18) = wCHKDGT
051400060512      *
051500060512     c                   ENDSR
051600060512
051700060512      *---------------------------------------------------------------*
051800060516      *?Impostazione dati del "DOVE" nell'etichetta DPD              ?*
051900060512      *---------------------------------------------------------------*
052000060512     c     sr_DPDdove    BEGSR
052100060512      *
052200060518     c                   eval      DP3NSPds = §DEeur
052300060518     c                   eval      DP3XX1ds = §DExx1
052400060518     c                   eval      DP3AWBds = §DEawb
052500060518     c                   eval      wDPDc1   = wDPDc1w
052600060518     c                   eval      wDPDc3   = wDPDc3w
052700060518     c                   eval      wDPDc5   = wDPDc5w
052800060518     c                   eval      wDPDc7   = wDPDc7w
052900060518     c                   eval      dsDPDn   =
053000060518     c                             'Notifications on damages which are -
053100040520     c                             not recognizable from the outside -
053200000118     c                             have to be submitted to the transport -
053300040521     c                             organisation within 7 days in written.'
053400060512      *
053500060512     c                   ENDSR
053600060512
053700060512      *---------------------------------------------------------------*
053800060516      *?Impostazione dati del "CHI SONO" nell'etichetta DPD          ?*
053900060512      *---------------------------------------------------------------*
054000060512     c     sr_DPDchi     BEGSR
054100060512      *
054200060512     c                   movel     §DExx2        DPDteln
054300060512     c                   move      §DExx2        DPDfaxn
054400040614     c                   movel     §LDlod        WLDlod
054500060517     c                   movel     §LDrsm        WLDrsm
054600060512      *
054700060512     c                   ENDSR
054800060518
054900060518      *---------------------------------------------------------------*
055000060518      *?Impostazione dati nell'etichetta DPD "DP4"                   ?*
055100060518      *---------------------------------------------------------------*
055200060518     c     sr_DP4        BEGSR
055300060523      *
055400060523      * Preparazione nominativo mittente   ("spezzato" in 2)
055500060523     c                   movel     §LDrsm        WLDrsm1
055600060525     c                   eval      WLDrsm2 = %trim(%subst(§LDrsm :
055700060525     c                                         %len(WLDrsm1) + 1 :
055800060525     c                                         %len(§LDrsm)  -
055900060525     c                                           %len(WLDrsm1)))
056000060607      * Impostazione nazione del deposito
056100060607     c                   movel     §DEsia        WDEsia
056200060518      *
056300060518      * Preparazione BarCode+CheckDigit
056400060518     c                   movel(p)  §DEeur        dsDP4_BarCode
056500060518     c                   movel     §DEsip        DP4bc_ChkD
056600060518      *
056700060518      * Preparazione Parcel/Tracking_number+CheckDigit
056800060518     c                   movel(p)  DP4bc_TrackNr dsDP4_Parcel
056900060518     c                   move      §DEsip        DP4pn_ChkD
057000061010      *
057100061010      * Impostazione "shipping-path" (IAT code + Priority group)
057200061010      *   nel "destination text"
057300061010     c                   if        %subst(§DEawb:1:4) > *blanks
057400061010     c                   eval      wDP4shpp = '-' + %subst(§DEawb:1:4)
057500061010     c                   endif
057600060518      *
057700060518      * Impostazione n° di telefono Bartolini
057800060518     c                   movel     §DExx2        DPDteln
057900060518      *
058000060518      * Impostazione n° di fax      Bartolini
058100060518     c                   movel     §LDcdp        DPDfaxn
058200060518      *
058300060518      * Decodifica Service Code
058400060518     c                   move      §DExx2        wDPDsrvc
058500060518      *
058600060518      * Preparazione dati da XX1
058700060518     c                   eval      DP4XX1ds = §DExx1
058800060626      *
058900060626      * Impostazione indicatori utilizzati in stapa_
059000060626      * - 61 = sigla servizio <> *blanks (box da stampare)
059100060626     c                   eval      *in61 = (§DEsse  <> *blanks)
059200060518      *
059300060518     c                   ENDSR
059400060511
059500060517      *===============================================================*
059600060517      *?ETICHETTA ITALIA   &   EUROEXPRESS (EEX) - v. *in55          ?*
059700060517      *===============================================================*
059800060512      ********* INIZIALIZZAZIONE ETICHETTA
059900060512     oFNLV22P   e            LV22             01
060000060512     o                                              '^XA'
060100060512      * - rotazione 180°
060200060512     o          e            LV22        1
060300060512     o                                              '^POI'
060400060512     o          e            LV22        1
060500111006     o                                              '^LH170,30'
060600060511      *** (margine senza rotazione)
060700111005     o***                                           '^LH120,60'
060800060512     o          e            LV22        1
060900060516     o                                              '^PRC^MSY'
061000060516     o                       C_end
061100111005      *
061200111005      ********* Mittente [in verticale]
061300060512     o          e            LV22D1      1
061400111005     o                                              '^FT888,750'
061500111005     o                                              '^A0B,38,33'
061600060512     o                                              '^FD'
061700060512     o                       §RI
061800060516     o                       C_end
061900111005      *
062000111005      ********* Destinatario (Rag.Soc.) [in verticale]
062100060512     o          e            LV22D4      1
062200111005     o                                              '^FO893,195'
062300111005     o                                              '^GB0,525,5'
062400060516     o                       C_end
062500060512     o          e            LV22D4      1
062600111005     o                                              '^FT938,750'
062700111005     o                                              '^A0B,38,33'
062800060512     o                                              '^FD'
062900060512     o                       §RI
063000060516     o                       C_end
063100111005      *
063200111005      ********* Destinatario (Località) [in verticale]
063300060512     o          e            LV22D5      1
063400111005     o                                              '^FT983,750'
063500111005     o                                              '^A0B,38,33'
063600060512     o                                              '^FD'
063700060512     o                       §RI
063800060516     o                       C_end
063900111005      *****  e  Volume spedizione [in verticale]
064000060512     o          e            LV22D5      1
064100111005     o                                              '^FT1035,720'
064200111005     o                                              '^A0B,38,33'
064300060512     o                                              '^FD'
064400060512     o                                              'MC.'
064500060512     o                                              ' '
064600060512     o                       §LDvol                 '0 ,   '
064700060512     o                                              ' '
064800060512     o                       §LDfvr
064900060516     o                       C_end
065000111005      *
065100060512      ********* SERVIZIO
065200060512     o          e            LV22X5      1
065300111005     o                                              '^FO000,360'
065400111005     o                                              '^GB098,098,05'
065500060516     o                       C_end
065600060512     o          e            LV22X5      1
065700111005     o                                              '^FO030,375'
065800111005     o                                              '^A0N,090,072'
065900060512     o                                              '^FD'
066000060512     o                       §LDtsp
066100060516     o                       C_end
066200111005      *
066300060512     o          e            LV22X5E     1
066400111005     o                                              '^FO000,360'
066500111005     o                                              '^GB098,098,90'
066600060516     o                       C_end
066700060512     o          e            LV22X5E     1
066800111005     o                                              '^FO030,375'
066900111005     o                                              '^A0N,090,072'
067000060512     o                                              '^FR'
067100060512     o                                              '^FD'
067200060512     o                       §LDtsp
067300060516     o                       C_end
067400111005      *
067500060512      ********* Linea Partenza
067600060512     o          e            LV22H       1
067700111005     o                                              '^FO000,480'
067800111005     o                                              '^ACN,81,30'
067900060512     o                                              '^FD'
068000060512     o                       §LDlnp
068100060516     o                       C_end
068200111005      *****  e  Sequenza colli [in verticale]
068300060512     o          e            LV22H       1
068400111005     o                                              '^FT1035,480'
068500111005     o                                              '^A0B,38,33'
068600060512     o                                              '^FD'
068700060512     o                       §RX
068800060516     o                       C_end
068900111005      *
069000111005      ********* SEGNACOLLO
069100060512     o          e    19      LV22D2      1
069200111006     o                                              '^FO000,555'
069300111005     o                                              '^A0N,130,100'
069400060512     o                                              '^FD'
069500060512     o                       wLDnrt
069600060516     o                       C_end
069700060512     o          e            LV22D2      1
069800111005     o                                              '^FO190,555'
069900111005     o                                              '^A0N,130,100'
070000060512     o                                              '^FD'
070100060512     o                       §S4
070200060516     o                       C_end
070300060512     o          e            LV22D2      1
070400111005     o                                              '^FO430,510'
070500111005     o                                              '^A0N,215,250'
070600060512     o                                              '^FD'
070700060512     o                       §S3
070800060516     o                       C_end
070900111005      *
071000060512      ********* TERMINAL/LINEA/ZONA ARRIVO
071100060512      ******    Terminal di Arrivo
071200060512     o          e            LV22D3      1
071300111005     o                                              '^FO135,000'
071400111005     o                                              '^GB195,180,005'
071500060516     o                       C_end
071600060512     o          e            LV22D3      1
071700111005     o                                              '^FO150,015'
071800111005     o                                              '^A0N,203,108'
071900060512     o                                              '^FR'
072000060512     o                                              '^FD'
072100060512     o                       wLDtnaO
072200060516     o                       C_end
072300060512      *****  e  Linea di Arrivo
072400060512     o          e            LV22D3      1
072500111005     o                                              '^FO360,075'
072600111005     o                                              '^A0N,135,72'
072700060512     o                                              '^FD'
072800060512     o                       wLDlnaO
072900060516     o                       C_end
073000111005      *****  e  Codice Pista (non impostato)
073100111005     o*//       e            LV22D3      1
073200111005     o*//                                           '^FO360,000'
073300111005     o*//                                           '^A0N,090,72'
073400111005     o*//                                           '^FD'
073500111005     o*//                    Pista
073600111005     o*//                    C_end
073700111005      *****  e  Codice Pallet (non impostato)
073800111005     o*//       e            LV22D3      1
073900111005     o*//                                           '^FO480,000'
074000111005     o*//                                           '^A0N,068,72'
074100111005     o*//                                           '^FD'
074200111005     o*//                    Pallet
074300111005     o*//                    C_end
074400111005      *****  e  Codice Operatore (non impostato)
074500111005     o*//       e            LV22D3      1
074600111005     o*//                                           '^FO645,000'
074700111005     o*//                                           '^A0N,068,72'
074800111005     o*//                                           '^FD'
074900111005     o*//                    Operat
075000111005     o*//                    C_end
075100060512      *****  e  Zona di Arrivo    (*in55 = EuroEXpress)
075200161019     o*//       e   N55      LV22D3      1
075300161019     o          e            LV22D3      1
075400161020     o                                              '^FO820,000'
075500111005     o                                              '^GB165,180,05'
075600060516     o                       C_end
075700161019     o*//       e   N55      LV22D3      1
075800161019     o          e            LV22D3      1
075900161020     o                                              '^FO835,015'
076000111005     o                                              '^A0N,203,126'
076100111010     o                                              '^FR'
076200060512     o                                              '^FD'
076300060512     o                       wLDzscO
076400060516     o                       C_end
076500111005      *****  e  Nazione di Arrivo (*in55 = EuroEXpress)
076600111005     o          e    55      LV22d3      1
076700111005     o                                              '^FO495,060'
076800161020     o                                              '^GB320,120,5'
076900111005     o                       C_end
077000111005     o          e    55      LV22d3      1
077100111005     o                                              '^FO510,075'
077200161020     o                                              '^A0N,120,67'
077300111005     o                                              '^FD'
077400111005     o                       §DEisa
077500111005     o                       C_end
077600111005     o          e    55      LV22d3      1
077700161020     o                                              '^FO600,075'
077800161020     o                                              '^A0N,120,55'
077900111005     o                                              '^FD'
078000111005     o                       §LDcad
078100111005     o                       C_end
078200041112      *
078300060512      ********* BARCODE Code128
078400060512      *****     "Chiusura" superiore
078500060512     o          e            LV22D6      1
078600111005     o                                              '^FO135,185'
078700111005     o                                              '^GBO670,015,015'
078800060516     o                       C_end
078900060512      *****  e  Bar-Code Code128
079000060512     o          e            LV22D6      1
079100111005     o                                              '^BY5,5,0'
079200060512     o          e            LV22D6      1
079300111005     o                                              '^FO135,195'
079400111005     o                                              '^BCN,300,N,N,N'
079500060512     o                                              '^FD>;'
079600060512     o                       wCDbar
079700060516     o                       C_end
079800060512      *****  e  "Chiusura" inferiore
079900060512     o          e            LV22D6      1
080000111005     o                                              '^FO135,495'
080100111005     o                                              '^GBO670,008,008'
080200060516     o                       C_end
080300111005      *
080400060512      ********* CODICE PRODOTTO
080500060512     o          e            LV22D7      1
080600111005     o                                              '^FO000,690'
080700111005     o                                              '^ACN,27,15'
080800060512     o                                              '^FD'
080900060512     o                       wRif21
081000060516     o                       C_end
081100060512      *****  e  Data per stampa/RISTAMPA
081200060512     o          e            LV22D7      1
081300111005     o                                              '^FO520,690'
081400111005     o                                              '^ACN,27,15'
081500060512     o                                              '^FD'
081600060512     o                       DataSp                 '  /  /    '
081700060516     o                       C_end
081800060511      *****  e  Flag ristampa
081900060512     o          e            LV22D7      1
082000111005     o                                              '^FO780,690'
082100111005     o                                              '^ACN,27,15'
082200060512     o                                              '^FD'
082300060512     o                       wLDRST
082400060516     o                       C_end
082500060511      *****  e  2° flag ristampa "V"
082600060512     o          e            LV22D7      1
082700111005     o                                              '^FO810,675'
082800111005     o                                              '^ACN,54,25'
082900060512     o                                              '^FD'
083000060512     o                       wLDrst2
083100060516     o                       C_end
083200111005      *****  e  Peso in kg [in verticale]
083300111005     o          e            LV22D7      1
083400111005     o                                              '^FT1035,240'
083500111005     o                                              '^A0B,38,33'
083600111005     o                                              '^FD'
083700111005     o                                              'KG.'
083800111005     o                                              ' '
083900111005     o                       §LDpkg                 '    0 , '
084000111005     o                       C_end
084100111005      *
084200060512      ********* CHIUSURA ETICHETTA
084300060512     o          e            LV22D8      1
084400060512     o                                              '^XZ'
084500060517
084600060517      *===============================================================*
084700060516      *?ETICHETTA ESTERO DPD formato "DP3"                           ?*
084800060517      *===============================================================*
084900060512      * DPD: etichetta 3 grande
085000060512     o          e            LV22dpd3         01
085100060512     o                                              '^XA'
085200060512     o          e            LV22dpd3    1
085300060512     o                                              '^PON'
085400060512     o          e            LV22dpd3    1
085500060512     o                                              '^LH0,0'
085600060512     o          e            LV22dpd3    1
085700060512     o                                              '^PRC'
085800060512      *---*  Box Spedizione
085900060512     o          e            LV22dpd3    1
086000111005     o                                              '^FO255,030'
086100111005     o                                              '^GB885,158,006'
086200060516     o                       C_end
086300060512      *---*  Telefono
086400060512     o          e            LV22dpd3    1
086500111005     o                                              '^FO480,045'
086600111007     o                                              '^ACN,25,12'
086700060512     o                                              '^FD'
086800060512     o                       dsDPD2
086900060516     o                       C_end
087000040519      *---*  Fax
087100060512     o          e            LV22dpd3    1
087200111005     o                                              '^FO480,075'
087300111007     o                                              '^ACN,25,12'
087400060512     o                                              '^FD'
087500060512     o                       dsDPD2f
087600060516     o                       C_end
087700111007      *---*  Bartolini BRT S.p.a.
087800060512     o          e            LV22dpd3    1
087900111007     o                                              '^FO760,045'
088000111007     o                                              '^ADN,25,13'
088100060512     o                                              '^FD'
088200060512     o                       C_dpd10
088300060516     o                       C_end
088400060512     o          e            LV22dpd3    1
088500111005     o                                              '^FO900,090'
088600111007     o                                              '^ADN,27,15'
088700060512     o                                              '^FD'
088800060512     o                       §LDdla
088900060516     o                       C_end
089000060512     o          e            LV22dpd3    1
089100111005     o                                              '^FO900,135'
089200111007     o                                              '^ADN,27,15'
089300060512     o                                              '^FD'
089400060512     o                       C_dpd05
089500060516     o                       C_end
089600060512      *---*  Numero Spedizione
089700060512     o          e            LV22dpd3    1
089800111005     o                                              '^FO270,090'
089900111005     o                                              '^ACN,108,45'
090000060512     o                                              '^FD'
090100060518     o                       DP3nsp_3a
090200060516     o                       C_end
090300040518      * 8 caratteri numero spedizione
090400060512     o          e            LV22dpd3    1
090500111007     o                                              '^FO450,105'
090600111007     o                                              '^ACN,81,27'
090700060512     o                                              '^FD'
090800060518     o                       DP3nsp_8a
090900060516     o                       C_end
091000060512      * ultimo carattere numero spedizione
091100060512     o          e            LV22dpd3    1
091200111007     o                                              '^FO750,135'
091300111005     o                                              '^ACN,54,30'
091400060512     o                                              '^FD'
091500060518     o                       DP3nsp_1a
091600060516     o                       C_end
091700060512      *---*  Costante "Destinatario"
091800060512     o          e            LV22dpd3    1
091900111007     o                                              '^FO050,210'
092000111005     o                                              '^ACN,27,15'
092100060512     o                                              '^FD'
092200060512     o                       C_dpd04
092300060516     o                       C_end
092400060512      *---*  Destinatario
092500060512     o          e            LV22dpd3    1
092600111007     o                                              '^FO270,210'
092700111005     o                                              '^ACN,54,15'
092800060512     o                                              '^FD'
092900060512     o                       §LDrsd
093000060516     o                       C_end
093100060512      *---*  Indirizzo Destinatario
093200060512     o          e            LV22dpd3    1
093300111007     o                                              '^FO270,285'
093400111005     o                                              '^ACN,54,15'
093500060512     o                                              '^FD'
093600060512     o                       §LDind
093700060516     o                       C_end
093800060512      *---*  Cap Destinatario
093900060512     o          e            LV22dpd3    1
094000111007     o                                              '^FO075,360'
094100111005     o                                              '^A0N,75,60'
094200060512     o                                              '^FD'
094300060512     o                       §LDcad
094400060516     o                       C_end
094500111007      *---*  Località Destinatario
094600111007     o          e            LV22dpd3    1
094700111007     o                                              '^FO270,360'
094800111007     o                                              '^A0N,75,60'
094900111007     o                                              '^FD'
095000111007     o                       wLDlod
095100111007     o                       C_end
095200060512      *---*  Nazione Destinatario
095300060512     o          e            LV22dpd3    1
095400111007     o                                              '^FO1035,360'
095500111005     o                                              '^A0N,75,60'
095600060512     o                                              '^FD'
095700060512     o                       §DEsia
095800060516     o                       C_end
095900060512      *---*  linea superiore
096000060512     o          e            LV22dpd3    1
096100111007     o                                              '^FO020,450'
096200111007     o                                              '^GB1120,0,005'
096300060516     o                       C_end
096400060512      *---*  costante scritta
096500060512     o          e            LV22dpd3    1
096600111007     o                                              '^FO030,465'
096700111007     o                                              '^ABN,17,9'
096800060512     o                                              '^FD'
096900111007     o                       wDPDn12
097000060516     o                       C_end
097100060512     o          e            LV22dpd3    1
097200111007     o                                              '^FO030,495'
097300111007     o                                              '^ABN,17,9'
097400060512     o                                              '^FD'
097500111007     o                       wDPDn34
097600060516     o                       C_end
097700060512      *---*  linea inferiore
097800060512     o          e            LV22dpd3    1
097900111007     o                                              '^FO020,525'
098000111007     o                                              '^GB1120,0,005'
098100060516     o                       C_end
098200060512      *---*  chiusura superiore 1° B-C
098300111007     o          e            LV22dpd3    1
098400111007     o                                              '^FO025,630'
098500111007     o                                              '^GB705,0,012'
098600111007     o                       C_end
098700060512      *---*  costante primo B-C
098800060512     o          e            LV22dpd3    1
098900111007     o                                              '^FO025,630'
099000111007     o                                              '^BY6,3'
099100111007     o                                              '^B2N,330,N,N,N'
099200060512     o                                              '^FD'
099300060518     o                       DP3NSPds
099400060516     o                       C_end
099500060512      *---*  chiusura inferiore
099600111007     o          e            LV22dpd3    1
099700111007     o                                              '^FO025,960'
099800111007     o                                              '^GB705,0,012'
099900111007     o                       C_end
100000060518      *---*  Versione Cappario
100100060512     o          e            LV22dpd3    1
100200111005     o                                              '^FO300,990'
100300111005     o                                              '^ABN,50,32'
100400060512     o                                              '^FD'
100500060518     o                       wDP3xx1vcap
100600060516     o                       C_end
100700060512      *---*  chiusura superiore 2° B-C
100800111007     o          e            LV22dpd3    1
100900111007     o                                              '^FO090,1065'
101000111007     o                                              '^GB595,0,012'
101100111007     o                       C_end
101200060512      *---*  Codice a Barre
101300060512     o          e            LV22dpd3    1
101400111007     o                                              '^FO090,1065'
101500111007     o                                              '^BY6,3'
101600111007     o                                              '^B2N,360,N,N,N'
101700060512     o                                              '^FD'
101800060512     o                       §DEawb
101900060516     o                       C_end
102000060512      *---*  chiusura inferiore
102100111007     o          e            LV22dpd3    1
102200111007     o                                              '^FO090,1425'
102300111007     o                                              '^GB595,0,012'
102400111007     o                       C_end
102500060512      *---*  decodifica 2° bcc
102600060516     o          e            LV22dpd3    1
102700111005     o                                              '^FO195,1445'
102800111005     o                                              '^ABN,33,21'
102900060512     o                                              '^FD'
103000060518     o                       DP3DOVEds
103100060516     o                       C_end
103200060512      *---*  Costante "Mittente"
103300060512     o          e            LV22dpd3    1
103400111005     o                                              '^FT855,1230'
103500111005     o                                              '^A0B,45,36'
103600060512     o                                              '^FD'
103700060512     o                       C_dpd03
103800060516     o                       C_end
103900060512      *---*  Mittente
104000060512     o          e            LV22dpd3    1
104100111005     o                                              '^FT945,1230'
104200111005     o                                              '^A0B,68,54'
104300060512     o                                              '^FD'
104400060512     o                       wLDrsm
104500060516     o                       C_end
104600060512      *---*  Provincia Mittente
104700060512     o          e            LV22dpd3    1
104800111005     o                                              '^FT1035,1230'
104900111005     o                                              '^A0B,68,54'
105000060512     o                                              '^FD'
105100060512     o                       §LDprm
105200060516     o                       C_end
105300060512      *---*  box laterale mittente
105400060512     o          e            LV22dpd3    1
105500111005     o                                              '^FO750,525'
105600111005     o                                              '^GB390,1230,005'
105700060516     o                       C_end
105800060512      * linea interna box verticale
105900060512     o          e            LV22dpd3    1
106000111005     o                                              '^FO945,1260'
106100111005     o                                              '^GB0,405,005'
106200060516     o                       C_end
106300060512      * linea interna box laterale
106400060512     o          e            LV22dpd3    1
106500111005     o                                              '^FO750,1260'
106600111005     o                                              '^GB390,0,005'
106700060516     o                       C_end
106800060512      *---*  costanti primi quadranti
106900060512     o          e            LV22dpd3    1
107000111005     o                                              '^FO795,1275'
107100111007     o                                              '^ABN,11,07'
107200060512     o                                              '^FD'
107300060512     o                       C_dpd11
107400060516     o                       C_end
107500060512     o          e            LV22dpd3    1
107600111005     o                                              '^FO773,1305'
107700111007     o                                              '^ABN,11,07'
107800060512     o                                              '^FD'
107900060512     o                       C_dpd12
108000060516     o                       C_end
108100060512     o          e            LV22dpd3    1
108200111005     o                                              '^FO780,1335'
108300111007     o                                              '^ACN,18,10'
108400060512     o                                              '^FD'
108500060512     o                       C_dpd14
108600060516     o                       C_end
108700060512     o          e            LV22dpd3    1
108800111005     o                                              '^FO975,1305'
108900111007     o                                              '^ABN,11,07'
109000060512     o                                              '^FD'
109100060512     o                       C_dpd13
109200060516     o                       C_end
109300060512      *---*  linea interna box laterale 2
109400060512     o          e            LV22dpd3    1
109500111005     o                                              '^FO750,1395'
109600111005     o                                              '^GB390,0,005'
109700060516     o                       C_end
109800060512      *---*  costanti secondi quadranti
109900060516     o          e            LV22dpd3    1
110000111005     o                                              '^FO795,1425'
110100111007     o                                              '^ABN,11,07'
110200060512     o                                              '^FD'
110300060512     o                       C_dpd15
110400060516     o                       C_end
110500060516     o          e            LV22dpd3    1
110600111005     o                                              '^FO990,1425'
110700111007     o                                              '^ABN,11,07'
110800060512     o                                              '^FD'
110900060512     o                       C_dpd16
111000060516     o                       C_end
111100060512      *---*  Data Spedizione
111200060516     o          e            LV22dpd3    1
111300111005     o                                              '^FO953,1455'
111400111007     o                                              '^ADN,18,10'
111500060512     o                                              '^FD'
111600060512     o                       DataSp                 '  /  /    '
111700060516     o                       C_end
111800060512      *---*  linea interna box laterale 3
111900060512     o          e            LV22dpd3    1
112000111005     o                                              '^FO750,1530'
112100111005     o                                              '^GB390,0,005'
112200060516     o                       C_end
112300060512      *---*  costanti terzi quadranti
112400060516     o          e            LV22dpd3    1
112500111005     o                                              '^FO765,1560'
112600111007     o                                              '^ABN,11,07'
112700060512     o                                              '^FD'
112800060512     o                       C_dpd17
112900060516     o                       C_end
113000060512      *---*  Peso (kg)
113100060512     o          e            LV22dpd3    1
113200111005     o                                              '^FO773,1590'
113300111007     o                                              '^ADN,18,10'
113400060512     o                                              '^FD'
113500060512     o                       §LDpkg        2
113600060516     o                       C_end
113700060512     o          e            LV22dpd3    1
113800111005     o                                              '^FO968,1560'
113900111007     o                                              '^ABN,11,07'
114000060512     o                                              '^FD'
114100060512     o                       C_dpd18
114200060516     o                       C_end
114300060512      *---*  Linea interna box laterale 4
114400060512     o          e            LV22dpd3    1
114500111005     o                                              '^FO750,1665'
114600111005     o                                              '^GB390,0,005'
114700060516     o                       C_end
114800060512      *---*  quarto quadrante Rif. Sped. Bartolini
114900060512     o          e            LV22dpd3    1
115000111005     o                                              '^FO758,1688'
115100111005     o                                              '^ABN,50,21'
115200060512     o                                              '^FD'
115300060512     o                       §LDlnp
115400060512     o                                              ' '
115500060512     o                       §LDnrs
115600060512     o                                              ' '
115700060512     o                       §LDncd
115800060516     o                       C_end
115900060512      *---*  costante x
116000060512     o          e            LV22dpd3    1
116100111005     o                                              '^FO090,1530'
116200111005     o                                              '^ACN,108,45'
116300060512     o                                              '^FD'
116400060512     o                       §DEsse
116500060516     o                       C_end
116600060512      *---*  costante 09
116700060512     o          e            LV22dpd3    1
116800111005     o                                              '^FO090,1665'
116900111005     o                                              '^ACN,108,45'
117000060512     o                                              '^FD'
117100060512     o                       C_dpd09
117200060516     o                       C_end
117300060512      *---*  Linea Arrivo
117400060512     o          e            LV22dpd3    1
117500111007     o                                              '^FO240,1610'
117600111005     o                                              '^ACN,189,60'
117700060512     o                                              '^FD'
117800060518     o                       wDP3xx1lna
117900060516     o                       C_end
118000060512      *---*  Zona Arrivo
118100060512     o          e            LV22dpd3    1
118200111005     o                                              '^FO503,1665'
118300111005     o                                              '^ACN,108,45'
118400060512     o                                              '^FD'
118500060512     o                                              '. '
118600060518     o                       wDP3xx1zna
118700060516     o                       C_end
118800060519      *---*  FINE
118900060512     o          e            LV22dpd3    1
119000060512     o                                              '^XZ'
119100060517
119200060517      *===============================================================*
119300060516      *?ETICHETTA ESTERO DPD formato "DP4"                           ?*
119400060517      *===============================================================*
119500060516      * DPD: etichetta 4 grande
119600060523      *
119700060523      *===* START?
119800060523      *
119900060519      *---* ?INIZIO etichetta?
120000060516     o          e            LV22dpd4         01
120100060516     o                                              '^XA'
120200060516     o          e            LV22dpd4    1
120300060516     o                                              '^PON'
120400060516     o          e            LV22dpd4    1
120500060516     o                                              '^LH0,0'
120600060516     o          e            LV22dpd4    1
120700060516     o                                              '^PRC'
120800060523      *
120900060523      *===* SHIPMENT INFORMATION FIELD?
121000060518      *
121100060518      *---* ?costante: Notification of Dameges?
121200060516     o          e            LV22dpd4    1
121300111006     o                                              '^FO260,045'
121400111006     o                                              '^ACN,23,15'
121500060516     o                                              '^FD'
121600060518     o                       C_dpd00a
121700060516     o                       C_end
121800060516     o          e            LV22dpd4    1
121900111006     o                                              '^FO260,065'
122000111006     o                                              '^ACN,23,15'
122100060516     o                                              '^FD'
122200060518     o                       C_DPD00b
122300060516     o                       C_end
122400060516     o          e            LV22dpd4    1
122500111006     o                                              '^FO260,090'
122600111006     o                                              '^ACN,23,15'
122700060516     o                                              '^FD'
122800060518     o                       C_dpd00c
122900060516     o                       C_end
123000060519     o          e            LV22dpd4    1
123100111006     o                                              '^FO260,113'
123200111006     o                                              '^ACN,23,15'
123300060519     o                                              '^FD'
123400060519     o                       C_dpd00d
123500060519     o                       C_end
123600060518      *
123700060517      *---*  _ linea orizzontale separatrice
123800060516      *      Notification of Damages / Delivery Address
123900060516     o          e            LV22dpd4    1
124000111005     o                                              '^FO030,165'
124100111006     o                                              '^GB1120,0,005'
124200060516     o                       C_end
124300060517      *---*  | linea verticale separatrice
124400060523      *      Delivery Address / Consignor Address (Sender)
124500060517     o          e            LV22dpd4    1
124600111005     o                                              '^FO833,165'
124700111005     o                                              '^GB0,510,005'
124800060517     o                       C_end
124900060517      *---*  | linea verticale separatrice
125000060523      *      Consignor Address (Sender) / dispatching depot address
125100060517     o          e            LV22dpd4    1
125200111005     o                                              '^FO975,165'
125300111005     o                                              '^GB0,510,005'
125400060517     o                       C_end
125500060523      *---*  _ linea orizzontale separatrice
125600060523      *      Delivery Address / Customer specific details (References)
125700060523     o          e            LV22dpd4    1
125800111005     o                                              '^FO030,450'
125900111006     o                                              '^GB803,0,005'
126000060523     o                       C_end
126100060523      *---*  | linea verticale separatrice
126200060523      *      Customer specific details (Referces) / Parcel details
126300060523     o          e            LV22dpd4    1
126400111005     o                                              '^FO615,450'
126500111005     o                                              '^GB0,225,005'
126600060523     o                       C_end
126700060518      *
126800060518      *---* ?DELIVERY ADDRESS?
126900060517      * - -  costante: Delivery Address (verticale)
127000060517     o          e            LV22dpd4    1
127100111005     o                                              '^FO795,180'
127200111005     o                                              '^A0R,30,23'
127300060517     o                                              '^FD'
127400060517     o                       C_dpd01
127500060517     o                       C_end
127600060517      * - -  Nominativo Destinatario
127700060517     o          e            LV22dpd4    1
127800111006     o                                              '^FO030,195'
127900111010     o                                              '^ADN,38,10'
128000060517     o                                              '^FD'
128100060517     o                       §LDrsd
128200060517     o                       C_end
128300060518      * - -  Indirizzo Destinatario
128400060517     o          e            LV22dpd4    1
128500111006     o                                              '^FO030,255'
128600111010     o                                              '^ADN,38,10'
128700060517     o                                              '^FD'
128800060517     o                       §LDind
128900060517     o                       C_end
129000060518      * - -  Nazione-Cap Destinatario
129100060518     o          e            LV22dpd4    1
129200111005     o                                              '^FO060,315'
129300111005     o                                              '^ACN,45,30'
129400060518     o                                              '^FD'
129500060518     o                       §DEisa
129600060518     o                                              '-'
129700060518     o                       §LDcad
129800060518     o                       C_end
129900060518      * - -  Località Destinatario
130000060517     o          e            LV22dpd4    1
130100111006     o                                              '^FO030,390'
130200111010     o*//                                           '^ACN,45,10'
130300111010     o                                              '^ADN,38,10'
130400060523     o                                              '^FD'
130500060530     o                       §LDlod
130600060523     o                       C_end
130700060518      *
130800060518      *---* ?SENDER?(verticale)
130900060517      * - -  costante: Sender (verticale)
131000060517     o          e            LV22dpd4    1
131100111005     o                                              '^FO938,180'
131200111005     o                                              '^A0R,30,23'
131300060517     o                                              '^FD'
131400060517     o                       C_dpd03
131500060517     o                       C_end
131600060517      * - -  Nominativo Mittente (verticale)
131700060517     o          e            LV22dpd4    1
131800111005     o                                              '^FO908,195'
131900111006     o                                              '^ACR,23,15'
132000060517     o                                              '^FD'
132100060523     o                       wLDrsm1
132200060517     o                       C_end
132300060523     o          e            LV22dpd4    1
132400111005     o                                              '^FO878,195'
132500111006     o                                              '^ACR,23,15'
132600060523     o                                              '^FD'
132700060523     o                       wLDrsm2
132800060523     o                       C_end
132900060517      * - -  Provincia Mittente (verticale)
133000060517     o          e            LV22dpd4    1
133100111005     o                                              '^FT848,195'
133200111005     o                                              '^ACR,23,15'
133300060517     o                                              '^FD'
133400060517     o                       §LDprm
133500060517     o                       C_end
133600111005      *---* ?CORRIERE "BRT S.p.A."?(verticale)
133700111005      * - -  costante: BRT S.p.A.
133800060517     o          e            LV22dpd4    1
133900111005     o                                              '^FO1080,195'
134000111005     o                                              '^ACR,23,12'
134100060517     o                                              '^FD'
134200060517     o                       C_dpd10
134300060517     o                       C_end
134400060517      * - -  P.O. Bartolini
134500060517     o          e            LV22dpd4    1
134600111005     o                                              '^FO1050,195'
134700111005     o                                              '^ACR,20,08'
134800060517     o                                              '^FD'
134900060517     o                       §LDdla
135000060517     o                       C_end
135100060517      * - -  costante: Italy
135200060517     o          e            LV22dpd4    1
135300111005     o                                              '^FO1050,465'
135400111005     o                                              '^ACR,20,08'
135500060517     o                                              '^FD'
135600060517     o                       C_dpd05
135700060517     o                       C_end
135800060517      * - -  Telefono
135900060517     o          e            LV22dpd4    1
136000111005     o                                              '^FO1020,195'
136100111005     o                                              '^ACR,20,08'
136200060517     o                                              '^FD'
136300060517     o                       DPDtel
136400060517     o                                              '. '
136500060517     o                       DPDteln
136600060517     o                       C_end
136700060517      * - -  Fax
136800060517     o          e            LV22dpd4    1
136900111005     o                                              '^FO990,195'
137000111005     o                                              '^ACR,23,12'
137100060517     o                                              '^FD'
137200060517     o                       DPDfax
137300060523     o                                              '  '
137400060518     o                       §LDcdp
137500060517     o                       C_end
137600060518      *
137700060518      *---* ?CUSTOMER SPECIFIC DETAILS?
137800060518      * - -  costante: Riferimenti
137900060517     o          e            LV22dpd4    1
138000111005     o                                              '^FO060,465'
138100111005     o                                              '^A0N,30,23'
138200060517     o                                              '^FD'
138300060518     o                                              'References:'
138400060517     o                       C_end
138500060518      * - -  Numero Segnacollo (LNP+NRS+NCD)
138600060518     o          e            LV22dpd4    1
138700111005     o                                              '^FO060,510'
138800111005     o                                              '^ADN,60,30'
138900060518     o                                              '^FD'
139000060518     o                       §LDlnp
139100060518     o                                              ' '
139200060518     o                       §LDnrs
139300060518     o                                              ' '
139400060518     o                       §LDncd
139500060518     o                       C_end
139600060518     *** * - -  costante: ...
139700060518     ***o          e            LV22dpd4    1
139800111005     ***o                                              '^FO060,555'
139900111005     ***o                                              '^A0N,30,15'
140000060518     ***o                                              '^FD'
140100060518     ***o                                              ' '
140200060518     ***o                       C_end
140300060518      *---* ?PARCEL DETAILS?
140400060517      * - -  costante: Collo  Nr / Di
140500060517     o          e            LV22dpd4    1
140600111005     o                                              '^FO660,465'
140700111005     o                                              '^A0N,30,23'
140800060517     o                                              '^FD'
140900060518     o***                                           'Parcel Nr / Of'
141000060518     o                                              'Lieferung'
141100060517     o                       C_end
141200060517      * - -  costante: 1 / 1
141300060517     o          e            LV22dpd4    1
141400111005     o                                              '^FO660,510'
141500111005     o                                              '^A0N,45,38'
141600060517     o                                              '^FD'
141700060523     o                                              '  1 / 1  '
141800060517     o                       C_end
141900060517      * - -  costante: Peso
142000060517     o          e            LV22dpd4    1
142100111005     o                                              '^FO660,570'
142200111005     o                                              '^A0N,30,23'
142300060517     o                                              '^FD'
142400060518     o***                                           'Weight'
142500060519     o                                              'Gewicht'
142600060517     o                       C_end
142700060517      * - -  Peso in kg
142800060517     o          e            LV22dpd4    1
142900111005     o                                              '^FO630,615'
143000111005     o                                              '^A0N,45,38'
143100060517     o                                              '^FD'
143200060531     o                       §LDpkg        3
143300060517     o                                              ' kg'
143400060517     o                       C_end
143500060523      *
143600060523      *===* SERVICE FIELD?
143700060518      *
143800060517      *---*  _ linea orizzontale separatrice
143900060523      *      Delivey Address / Service field
144000060516     o          e            LV22dpd4    1
144100111005     o                                              '^FO030,675'
144200111006     o                                              '^GB1120,0,005'
144300060516     o                       C_end
144400060518      *
144500060517      *---*  _ linea orizzontale separatrice
144600060523      *      Service field / Route filed
144700060517     o          e            LV22dpd4    1
144800111005     o                                              '^FO030,930'
144900111006     o                                              '^GB1120,0,005'
145000060517     o                       C_end
145100060523      *
145200060523      *===* ROUTE FIELD?
145300060518      *
145400060518      *---* ?TRACKING NUMBER?
145500060518      * . .  Numero Parcel
145600060518     o          e            LV22dpd4    1
145700111006     o                                              '^FO060,975'
145800111005     o                                              '^A0N,75,90'
145900060518     o                                              '^FD'
146000060518     o                       DP4pn_a
146100060518     o                       C_end
146200060518     o          e            LV22dpd4    1
146300111006     o                                              '^FO255,975'
146400111005     o                                              '^A0N,60,60'
146500060518     o                                              '^FD'
146600060518     o                       DP4pn_b
146700060518     o                                              ' '
146800060518     o                       DP4pn_c
146900060518     o                                              ' '
147000060518     o                       DP4pn_d
147100060518     o                       C_end
147200060518     o          e            LV22dpd4    1
147300111006     o                                              '^FO600,975'
147400111005     o                                              '^A0N,45,45'
147500060518     o                                              '^FD'
147600060518     o                       DP4pn_ChkD
147700060518     o                       C_end
147800060523      *
147900060523      *---* ?SERVICE CODE?
148000060523     o          e            LV22dpd4    1
148100111005     o                                              '^FO540,975'
148200111005     o                                              '^ADN,45,30'
148300060523     o                                              '^FD'
148400060523     o                       wDPDsrvc
148500060523     o                       C_end
148600060523      *
148700060523      *---*  costante: Track
148800060523     o          e            LV22dpd4    1
148900111005     o                                              '^FO060,1050'
149000111005     o                                              '^A0N,30,23'
149100060518     o                                              '^FD'
149200060518     o                                              'Track'
149300060518     o                       C_end
149400060523      *---*  costante: Servizio
149500060519     o          e            LV22dpd4    1
149600111005     o                                              '^FO1035,1050'
149700111005     o                                              '^A0N,30,23'
149800060518     o                                              '^FD'
149900060518     o                                              'Service'
150000060518     o                       C_end
150100060518      *
150200060518      *---* ?IDENTIFICATION?
150300060518      *      Sigla servizio: "X" = small parcel, "E" = express parcel
150400060626     o          e            LV22dpd4    1
150500111006     o                                              '^FO075,1110'
150600111005     o                                              '^ADN,75,38'
150700060518     o                                              '^FD'
150800060518     o                       §DEsse
150900060518     o                       C_end
151000060626      *     ?   ?
151100060626      *---* ?| |?box per sigla servizio (identification)
151200060626     o          e    61      LV22dpd4    1
151300111006     o                                              '^FO060,1095'
151400111005     o                                              '^GB075,095,005'
151500060626     o                       C_end
151600060518      *
151700060518      *---* ?DESTINATION TEXT?
151800060523     o          e            LV22dpd4    1
151900111006     o                                              '^FO180,1095'
152000111005     o                                              '^ADN,150,63'
152100060518     o                                              '^FD'
152200060607     *** * . .  Destination-Country code
152300060607     ***o                       §DEisa
152400060607      * . .  Depot Country code
152500060607     o                       wDEsia
152600060518      * . .  Recipient Depot code
152700060518     o                                              '-'
152800060518     o                       wDP4xx1depo
152900061010      * . .  Shipping path
153000061010     o*** solo se shipping-path <> *blanks:         '-'
153100061010     o                       wDP4shpp
153200060518     o                       C_end
153300060518      *
153400060518      *---* ?SERVICE CODE-COUNTRY-RECIPIENT-ZIP?
153500060518     o          e            LV22dpd4    1
153600111006     o                                              '^FO250,1253'
153700111005     o                                              '^ADN,42,21'
153800060518     o                                              '^FD'
153900060518      * . .  Service code
154000060518     o                       DP4bc_SrvCd
154100060518      * . .  Destination-country code (nazione destinatario)
154200060523     o                                              ' - '
154300060518     o                       §DEisa
154400060518      * . .  Destination-postal code  (cap destinatario)
154500060523     o                                              ' - '
154600060518     o                       §LDcad
154700060518     o                       C_end
154800060518      *
154900060518      *---* ?VERSIONE CAPPARIO?
155000060518     o          e            LV22dpd4    1
155100111005     o                                              '^FO675,1260'
155200111005     o                                              '^ADN,23,11'
155300060518     o                                              '^FD'
155400060523     o                       wDP4xx1vcap
155500060518     o                       C_end
155600060518      *
155700060518      *---* ?DESTINATION SORT?
155800060518     o          e            LV22dpd4    1
155900111006     o                                              '^FO890,1245'
156000111005     o                                              '^ADN,75,45'
156100060518     o                                              '^FD'
156200060523     o                       wDP4xx1dsor
156300060518     o                       C_end
156400060523      *
156500060523      *===* BARCODE FIELD?
156600060518      *
156700060518      *---* ?BARCODE Code128?
156800060518      * - -  _ linea orizzontale superiore
156900060518     o          e            LV22dpd4    1
157000111006     o                                              '^FO080,1320'
157100111006     o                                              '^GB975,0,009'
157200060518     o                       C_end
157300060518      * - -  Codice a barre (con identification tag, senza check-digit)
157400060518     o          e            LV22dpd4    1
157500111006     o                                              '^FO080,1328'
157600111006     o                                              '^BY4,,300'
157700060525     o          e            LV22dpd4    1
157800111006     o                                              '^BCN,300,N,N,N'
157900060518     o                                              '^FD'
158000060529     o*** per inizio con campi B-alfanumerici:      '>:'
158100060529     o*** per inizio con campi C-numerici:          '>;'
158200060529     o                                              '>:'
158300060529     o                       DP4bc_IdTag
158400060529     o                       DP4bc_DPC
158500060530     o*** per campi alfanumerici dopo numerici:     '>6'
158600060530     o*** per campi numerici dopo alfanumerici:     '>5'
158700060530     o                                              '>5'
158800060627     o                       DP4bc_TrackNr
158900060529     o                       DP4bc_SrvCd
159000060529     o                       DP4bc_DCC
159100060529     o                       C_end
159200060518      * - -  Decodifica del Codice a barre (senza identification tag,
159300060518      *                                     con   check-digit)
159400060518     o          e            LV22dpd4    1
159500111005     o                                              '^FO090,1665'
159600111005     o                                              '^ABN,33,21'
159700060518     o                                              '^FD'
159800060523     o                       DP4bc_01
159900060518     o                                              ' '
160000060518     o                       DP4bc_02
160100060518     o                                              ' '
160200060518     o                       DP4bc_03
160300060518     o                                              ' '
160400060518     o                       DP4bc_04
160500060518     o                                              ' '
160600060518     o                       DP4bc_05
160700060518     o                                              ' '
160800060518     o                       DP4bc_06
160900060518     o                                              ' '
161000060518     o                       DP4bc_07
161100060518     o                                              ' '
161200060518     o                       DP4bc_08
161300060518     o                                              ' '
161400060529     o                       DP4bc_ChkD
161500060518     o                       C_end
161600060523      *
161700060523      *===* END?
161800060523      *
161900060519      *---* ?FINE etichetta?
162000060519     o          e            LV22dpd4    1
162100060519     o                                              '^XZ'
