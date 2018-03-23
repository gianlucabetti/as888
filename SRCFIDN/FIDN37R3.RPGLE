000100021024      *---------------------------------------------------------------*
000200060623      * Stampa Autodichiarazioni per non eventi                       *
000300021024      *---------------------------------------------------------------*
000400021024
000500060310     h decedit('0,') datedit(*dmy/)
000600021024
000700021025     fAZORG01L  if   e           k disk
000800021024     fFNBLP01L  if   e           k disk    prefix(ARB:3)
000900021024     fFNARB01L  if   e           k disk
001000060628     fFIAR601L  if   e           k disk
001100060627     fFNDCS01L  if a e           k disk
001200040804     fFNDCK01L  if   e           k disk
001300021024      *
001400021024     fFLNUF01L  uf   e           k disk
001500021024     fFNDCT01L  uf   e           k disk
001600021024      *
001700021024     fFNDCF00F  o    e           k disk
001800021024     fFNDET00F  o    e           k disk
001900021024      *
002000060623     fFIDN37P1  o    e             printer oflind(*in01) usropn
002100021029     f                                     infds(PrtfInf1)
002200060623     fFIDN37P2  o    e             printer oflind(*in02) usropn
002300021029     f                                     infds(PrtfInf2)
002400060623     fFIDN37P3  o    e             printer oflind(*in03) usropn
002500021029     f                                     infds(PrtfInf3)
002600021024
002700021024      *---------------------------------------------------------------*
002800021024
002900021024      *
003000021024      * COSTANTI -----------------------------------------------------*
003100021024      *
003200021024     d K_Mai           c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
003300021024     d K_Min           c                   const('abcdefghijklmnopqrstuvwxyz')
003400021024      *
003500021024      * SCHIERE A TEMPO DI COMPILAZIONE ------------------------------*
003600021024      *
003700021025     d MSG             s             78    dim(3) ctdata perrcd(1)
003800021024      *
003900021024      * SCHIERE A TEMPO DI ESECUZIONE   ------------------------------*
004000021024      *
004100021029     d $DesPM          s                   like(DCSnot) dim(930) inz
004200040804     D L6              S              3  0 DIM(30)                              P.O. GESTITI
004300021024      *
004400021024      * DS DEFINITE ESTERNAMENTE -------------------------------------*
004500021024      *
004600040804      *
004700040804     D TRUL06DS      E DS
004800040804     D  LIN                    1     90  0    DIM(30)                           P.O. COMODO
004900040804      *
005000021024     d KPJBA         e ds
005100060623     d FIDN37DS      e ds
005200090107     d FIDNBEDS      e ds
005300021025     d DDCT01        e ds                  inz
005400021025     d DS_FNARB      e ds                  inz extname(FNARB00F)
005500021025     d TIBS69DS      e ds                  inz
005600021025     d DS_cnaco      e ds                  inz extname(CNACO00F)
005700021025     d DS_cnind      e ds                  inz extname(CNIND00F)
005800021025     d DS_cnclp      e ds                  inz extname(CNCLP00F)
005900021025     d DS_fncls      e ds                  inz extname(FNCLS00F)
006000021025      *
006100021025      * DS DEFINITE INTERNAMENTE -------------------------------------*
006200021029      *
006300021029     d PrtfInf1        ds
006400021029     d  Cur_Line_1           367    368I 0
006500021029     d PrtfInf2        ds
006600021029     d  Cur_Line_2           367    368I 0
006700021029     d PrtfInf3        ds
006800021029     d  Cur_Line_3           367    368I 0
006900021025      *
007000060626     d WLBDAT          ds
007100021025     d  g02dat                 1      8  0
007200021025     d  g02inv                 9     16  0
007300021025     d  g02err                17     17
007400021025     d  g02tgi                18     22  0
007500021025      *
007600021025     d DSnumCA         ds                  inz
007700021025     d  DCTaac
007800021025     d  DCTfil
007900021025     d  DCTnca
008000021024      *
008100021024      * VARIABILI ----------------------------------------------------*
008200021024      *
008300021024     d w0140           s             14  0
008400021024     d wORA            s              4  0
008500021024     d wDATA           s              8  0
008600021024     d wDTGIO          s              8  0
008700021024     d $Err            s              3  0 inz
008800021029     d $Note           s              1    inz(*off)
008900021024     d XX              s              3  0 inz
009000021025     d DATA_eur        s               d   datfmt(*eur)   inz
009100021028     d wCLI            s                   like(ARBksc)   inz
009200090107     d WDS_KSC         s                   like(ARBksc)   inz
009300021028     d w0040           s              4  0 inz
009400060628     d wPO             s              3  0 inz
009500060310     d wCLSflo_7       s              1    inz
009600060627     D Ktpd            S                   LIKE(DCStpd)
009700060627     D Knkt            S                   LIKE(DCSnkt)
009800060627     D Kstd            S                   LIKE(DCSstd)
009900060627     D Kdim            S                   LIKE(DCSdim)
010000060627     D Khim            S                   LIKE(DCShim)
010100060627     D Knks            S                   LIKE(DCSnks)
010200060627     D Ktrc            S                   LIKE(DCStrc)
010300060628     d w§DCTport       s                   like(§DCTport) inz
010400021024
010500060627
010600060627     d prgnot          s                   like(dcspno)   inz(999)              progressivo note
010700021024      *---------------------------------------------------------------*
010800021024
010900021024      *****************************************************************
011000021024      *  RIEPILOGO INDICATORI
011100021024      *****************************************************************
011200021029      * 01-04 - OverFlow
011300021025      * 30    - Generico
011400021025      * 41-43 - Condizionano la stampa della merce mancante nelle 3
011500021025      *         diverse unità di misura: Colli, Pezzi e Kg.
011600060630      * 44    - Condiziono la stampa della descrizione solo per mancanze
011700060630      *****************************************************************
011800060626
011900060626      * Definizioni Chiavi
012000060626
012100060626     c     K03DCT01      Klist
012200060626     c                   Kfld                    I37aac
012300060626     c                   Kfld                    I37fil
012400060626     c                   Kfld                    I37nca
012500060626      *
012600060626     c     K04BLP01      Klist
012700060626     c                   Kfld                    DCTaas
012800060626     c                   Kfld                    DCTlnp
012900060626     c                   Kfld                    DCTnrs
013000060626     c                   Kfld                    DCTnsp
013100060628      *
013200060628     c     K05AR601      klist
013300060628     c                   Kfld                    DCTaas
013400060628     c                   Kfld                    DCTlnp
013500060628     c                   Kfld                    DCTnrs
013600060628     c                   Kfld                    DCTnsp
013700060628     c                   kfld                    AR6trc
013800060626      *
013900060626     c     K03DCS01      Klist
014000060626     c                   Kfld                    DCStpd
014100060626     c                   Kfld                    DCSnkt
014200060626     c                   Kfld                    DCSstd
014300060626      *
014400060627     C     KDCSE         klist
014500060627     C                   KFLD                    KTPD
014600060627     C                   KFLD                    KNKT
014700060627     C                   KFLD                    KSTD
014800060627     C                   KFLD                    KDIM
014900060627     C                   KFLD                    KHIM
015000060627     C                   KFLD                    KNKS
015100060627     C                   KFLD                    KTRC
015200060627
015300060627     c     kdcsp         KLIST
015400060627     c                   KFLD                    ktpd
015500060627     c                   KFLD                    knkt
015600060627     c                   KFLD                    kstd
015700060627     c                   KFLD                    kdim
015800060627     c                   KFLD                    khim
015900060627     c                   KFLD                    knks
016000060627     c                   KFLD                    ktrc
016100060627     c                   KFLD                    prgnot
016200060627      *
016300060628      * Apro il file stampa interessato
016400060628s   1c                   Select
016500060628    1c                   When      I37tde = '1'
016600060628if  2c                   if        not %open(FIDN37P1)
016700060628     c                   Open      FIDN37P1
016800060628e   2c                   endif
016900060628     c                   Clear                   DN37P1T
017000060628    1c                   When      I37tde = '2'
017100060628if  2c                   if        not %open(FIDN37P2)
017200060628     c                   Open      FIDN37P2
017300060628e   2c                   endif
017400060628     c                   Clear                   DN37P2T
017500060628    1c                   When      I37tde = '3'
017600060628if  2c                   if        not %open(FIDN37P3)
017700060628     c                   Open      FIDN37P3
017800060628e   2c                   endif
017900060628     c                   Clear                   DN37P3T
018000060628x   1c                   Other
018100060628     c                   Eval      $Err = 1
018200060628     c                   Exsr      EndPgm
018300060628e   1c                   Endsl
018400060627      *
018500060627      *
018600060627     C                   EXSR      CARL6
018700021024
018800021024      * Aggancio FNDCT per dati C.A. (da aggiornare)
018900021024     c     K03DCT01      Chain     FNDCT000
019000021025if  1c                   If        not %found(FNDCT01L)
019100021024     c                   Eval      $Err = 2
019200021024     c                   Exsr      EndPgm
019300021025e   1c                   Endif
019400060320
019500060320      * Reperisco dati Spedizione
019600060320      *
019700060320      * se CA con legami e se sono in arrivo rispetto la CA  recupero numero spedizione da FNDCK
019800060320     c                   movel     dctflo        ddct01
019900060320     c                   if        §dctlega = 'S'
020000060320     c     dctlna        lookup    L6                                     31
020100060320     c                   if        *in31
020200060320     c     K03dct01      chain     fndck01l
020300060320     c                   if        %found(fndck01l)
020400060320     c                   eval      dctaas = dckaas
020500060320     c                   eval      dctlnp = dcklnp
020600060320     c                   eval      dctnrs = dcknrs
020700060320     c                   eval      dctnsp = dcknsp
020800060320     c                   endif
020900060320     c                   endif
021000060320     c                   endif
021100060320      *
021200060320     c                   clear                   DS_FNARB
021300060320     c     K04BLP01      chain     FNBLP000                           30
021400060320if  1c                   if        not %found(FNBLP01L)
021500060320     c     K04BLP01      chain     FNARB000                           30
021600060320e   1c                   endif
021700060710      * non trovata bolla in filiale favo a fine programma
021800060710     c                   If        *in30
021900060710     c                   Exsr      EndPgm
022000060710e   1c                   Endif
022100060320
022200060320      * Reperisco i Dati del Beneficiario
022300060320     c                   Exsr      RepBenef
022400021024
022500021024      * Reperisco dati Punto Operativo
022600060623     c     I37fgs        Chain     AZORG
022700021025if  1c                   If            %found(AZORG01L)
022800021024     c                             and ORGfva = *blanks
022900021024     c                   Exsr      RieDatiPO
023000021025e   1c                   Endif
023100021024
023200021024      * Preparo campi di output
023300021029     c                   Exsr      RieVarLet
023400060628
023500021024
023600021024      * Stampo testo denuncia tipo lettera
023700021025s   1c                   Select
023800060623    1c                   When      I37tde = '1'
023900060626     c                   Write     DN37P1I
024000060626     c                   Write     DN37P1T
024100060623    1c                   When      I37tde = '2'
024200060626     c                   Write     DN37P2I
024300060626     c                   Write     DN37P2T
024400060623    1c                   When      I37tde = '3'
024500060626     c                   write     DN37P3I
024600060626     c                   Write     DN37P3T
024700021025e   1c                   Endsl
024800021029
024900021029      * Stampo note e chiusura lettera
025000021029      * - Note
025100021029     c                   Eval      $Note  =  *on
025200021029     c                   Clear                   xx
025300021029do  2c                   Dow       Xx <= %elem($DesPM) - 3
025400021029     c                   Exsr      PrtNOTE
025500021029e   2c                   Enddo
025600021029      * - Chiusura
025700021029     c                   Eval      $Note  =  *off
025800021029     c                   Exsr      Test_OF
025900021029s   2c                   Select
026000060623    2c                   When      I37tde = '1'
026100060626     c                   Write     DN37P1F
026200060623    2c                   When      I37tde = '2'
026300060626     c                   Write     DN37P2F
026400060626    2c                   When      I37tde = '3'
026500060626     c                   Write     DN37P3F
026600021029e   2c                   Endsl
026700021024
026800021024      * Aggiorno files
026900021025     c                   Exsr      AggDCT
027000060623     c                   Exsr      WrtDCS
027100021024
027200021024      * Fine
027300021024     c                   Exsr      EndPgm
027400021024
027500021024      *****************************************************************
027600021025      *  ROUTINE INIZIALE
027700021024      *****************************************************************
027800021024     c     *InzSR        BEGSR
027900021024      *
028000021024     c     *Entry        Plist
028100021024     c                   Parm                    KPJBA
028200021024      *
028300060623     c                   Movel     KPJBU         FIDN37ds
028400060627     c                   Clear                   O37err
028500060627     c                   Clear                   O37msg
028600060628      * verifico se tipo madalità  devo controllare il beneficiario
028700060628     c                   eval      *in07 = (i37mod = 'G')
028800021025      *
028900021024      * Reperisco Data e Ora
029000021024     c                   Time                    W0140
029100021024     c                   Movel     W0140         Wora
029200021024     c                   Move      W0140         Wdtgio
029300021024     c                   Z-add     Wdtgio        G02dat
029400021024     c                   Clear                   G02err
029500021024     c                   Call      'XSRDA8'
029600021024     c                   Parm                    WLBDAT
029700021024     c                   Movel     G02inv        Wdata
029800040804      *
029900021024     c                   ENDSR
030000040804      *****************************************************************
030100040804      * CARICO SCHIERA L6 DA TABELLA £6
030200040804      *****************************************************************
030300040804     C     carl6         BEGSR
030400040804      *
030500040804     C                   CLEAR                   L6
030600040804     C                   CLEAR                   TRUL06DS
030700040804     C                   MOVE      '£6'          D06COD
030800060623     C                   MOVEL     i37fgs        D06key
030900040804     C                   MOVEL(p)  TRUL06DS      KPJBU
031000040804      *
031100040804     C                   CALL      'TRUL06R'
031200040804     C                   PARM                    KPJBA
031300040804      *
031400040804     C                   MOVEL     KPJBU         TRUL06DS
031500040804     C                   MOVEA     LIN           L6
031600040804      *
031700040804     c                   endsr
031800021024
031900021024      *****************************************************************
032000021025      *  IMPOSTO DATI PUNTO OPERATIVO
032100021024      *****************************************************************
032200021024     c     RieDatiPO     BEGSR
032300021024      *
032400021028     c                   Movel     ORGdes        P1Tpof
032500060627     c                   Movel     ORGloc        P1Tloc1
032600021024      *
032700030320if  1c                   If        P1Tloc1 <> *blanks
032800030320     c                   Eval      P1Tloc1 = %trim(P1Tloc1) + ', lì '
032900060626     c                                     + %editc(Wdtgio:'Y')
033000030320e   1c                   Endif
033100021024      *
033200021024     c                   ENDSR
033300021024
033400021024      *****************************************************************
033500021025      *  IMPOSTO VARIABILI TESTO DENUNCIA  ** LETTERA **
033600021024      *****************************************************************
033700021029     c     RieVarLet     BEGSR
033800021025      *
033900021025      * Dati C.A.
034000021029     c                   Movel(p)  DCTfil        P1Tnca
034100021029     c                   Eval      P1Tnca = %trim(P1Tnca) + '-'
034200021029     c                                    + %trim(%editc(DCTnca:'Z'))
034300021028     c                   Z-add     DCTmgc        G02inv
034400021028     c                   Movel     DCTaac        G02inv
034500021028     c                   Movel     '3'           G02err
034600021028     c                   Call      'XSRDA8'
034700021028     c                   Parm                    WLBDAT
034800021028     c                   Z-add     G02dat        P1Tdca
034900021025     c                   Movel     DCTflo        DDCT01
035000060628     c                   movel     P1tnca        P1Tnca2
035100021025     c                   Z-add     DCTaac        P1Taac
035200021025     c                   Z-add     DCTmgc        G02inv
035300021025     c                   Movel     DCTaac        G02inv
035400021025     c                   Movel     '3'           G02err
035500021025     c                   Call      'XSRDA8'
035600021025     c                   Parm                    WLBDAT
035700021025     c                   Movel     G02dat        P1Tmgc
035800021025      *
035900021025      *
036000021025     c                   Z-add     DCTncn        P1Tncn
036100021025     c                   Z-add     DCTncn        P1Tncn2
036200021025     c                   Z-add     DCTpkd        P1Tpkd2
036300021025      *
036400021025      * Dati Spedizione
036500021025     c                   Z-add     ARBlnp        P1Tlnp
036600021025     c                   Z-add     ARBnrs        P1Tnrs
036700021025     c                   Z-add     ARBnsp        P1Tnsp
036800021025     c                   Z-add     ARBmgs        G02inv
036900021025     c                   Movel     ARBaas        G02inv
037000021025     c                   Movel     '3'           G02err
037100021025     c                   Call      'XSRDA8'
037200021025     c                   Parm                    WLBDAT
037300021025     c                   Z-add     G02dat        P1Tdsp
037400021025     c                   Z-add     ARBncl        P1Tncl
037500021025     c                   Z-add     ARBpkf        P1Tpkf
037600021025     c                   Z-add     ARBrmn        P1Trmn
037700021025     c                   Movel     ARBrsd        P1Trsd
037800021025      *
037900021025      * Merce Mancante
038000060630     c                   Movea     '0000'        *in(41)
038100021025     c                   if        DCTncn <> 0
038200021025     c                   Z-add     DCTncn        P1Tncn
038300021025     c                   Movel     *on           *in41
038400021025     c                   Endif
038500021025     c                   if        DCTnpz <> 0
038600021025     c                   Z-add     DCTnpz        P1Tnpz
038700021025     c                   Movel     *on           *in42
038800021025     c                   Endif
038900021025     c                   if        DCTpkd <> 0
039000021025     c                   Z-add     DCTpkd        P1Tpkd
039100021025     c                   Movel     *on           *in43
039200021025     c                   Endif
039300021025      *
039400021029      * SCHIERA Descrizione pezzi mancanti/danneggiati
039500021025     c                   Clear                   XX
039600021025     c                   Clear                   $DesPM
039700021025if  1c                   If        DCTnpz  > *zeros
039800021025     c                   Movel     'C'           DCStpd
039900021025     c                   Movel(p)  DSnumCA       DCSnkt
040000021025     c                   Movel     'P'           DCSstd
040100021025     c     K03DCS01      Chain     FNDCS000                           30
040200021025do  2c                   Dow       Not *in30  and  Xx < %elem($DesPM)
040300021025     c                   Add       1             xx
040400021025     c                   Movel     DCSnot        $DesPM(xx)
040500021025     c     K03DCS01      Reade     FNDCS000                               30
040600021025e   2c                   Enddo
040700021025e   1c                   Endif
040800060630      * se dichiarazione di smarrimento "3"  stampo la descrizione solo se c'è
040900060630      *
041000060630     c                   If        i37tde = '3' and xx > 0
041100060630     c                   eval      *in(44) = *on
041200060627     c                   endif
041300021024      *
041400021024     c                   ENDSR
041500021029
041600021029      *****************************************************************
041700021029      *  IMPOSTO NOTE IN STAMPA  &  STAMPO NOTE
041800021029      *****************************************************************
041900021029     c     PrtNote       BEGSR
042000021029      *
042100021029     c                   Add       1             xx
042200021029     c                   Movel     $DesPM(xx)    P1Tpdd1
042300021029     c                   Add       1             xx
042400021029     c                   Movel     $DesPM(xx)    P1Tpdd2
042500021029     c                   Add       1             xx
042600021029     c                   Movel     $DesPM(xx)    P1Tpdd3
042700021029      *
042800021029if  1c                   If           P1Tpdd1 <> *blanks
042900021029     c                             or P1Tpdd2 <> *blanks
043000021029     c                             or P1Tpdd3 <> *blanks
043100021029     c                   Exsr      Test_OF
043200021029s   2c                   Select
043300060626    2c                   When      I37tde = '1'
043400060626     c                   Write     DN37P1N
043500060626    2c                   When      I37tde = '2'
043600060626     c                   Write     DN37P2N
043700060626    2c                   When      I37tde = '3'
043800060626     c                   Write     DN37P3N
043900021029e   2c                   Endsl
044000021029e   1c                   Endif
044100021029      *
044200021029     c                   ENDSR
044300021029
044400021029      *****************************************************************
044500021029      *  CONTROLLO OVERFLOW
044600021029      *****************************************************************
044700021029     c     Test_OF       BEGSR
044800021029      *
044900021029s   1c                   Select
045000021029      *
045100060626    1c                   When      I37tde = '1'
045200021029if  2c                   If        $Note = *on
045300021030     c                   eval      *in01 = (Cur_Line_1 >= 61)
045400021029x   2c                   Else
045500021030     c                   eval      *in01 = (Cur_Line_1 >= 51)
045600021029e   2c                   Endif
045700021029if  2c                   If        *in01
045800060626     c                   Write     DN37P1I
045900021029     c                   Eval      *in01 = *off
046000021029e   2c                   Endif
046100021029      *
046200060626    1c                   When      I37tde = '2'
046300021029if  2c                   If        $Note = *on
046400021030     c                   eval      *in02 = (Cur_Line_2 >= 61)
046500021029x   2c                   Else
046600021030     c                   eval      *in02 = (Cur_Line_2 >= 51)
046700021029e   2c                   Endif
046800021029if  2c                   If        *in02
046900060626     c                   Write     DN37P2I
047000021029     c                   Eval      *in02 = *off
047100021029e   2c                   Endif
047200021029      *
047300060626    1c                   When      I37tde = '3'
047400021029if  2c                   If        $Note = *on
047500060627     c                   eval      *in03 = (Cur_Line_3 >= 61)
047600021029x   2c                   Else
047700060627     c                   eval      *in03 = (Cur_Line_3 >= 51)
047800021029e   2c                   Endif
047900060627if  2c                   If        *in03
048000060626     c                   Write     DN37P3I
048100060627     c                   Eval      *in03 = *off
048200021029e   2c                   Endif
048300021029      *
048400021029e   1c                   Endsl
048500021029      *
048600021029     c                   ENDSR
048700021025
048800021025      *****************************************************************
048900021025      *  IMPOSTO DATI CLIENTE BENEFICIARIO PER STAMPA
049000021025      *  AUTO-DICHIARAZIONE DEI P.O. DI SMARRIMENTO DELLA MERCE
049100021025      *****************************************************************
049200021025     c     RepBENEF      BEGSR
049300021025      *
049400090107     c                   clear                   wCLSflo_7
049500090107     c                   Clear                   wCli
049600090107      *
049700090107      * recupero beneficiario dal pgm esterno
049800090107     c                   clear                   fidnbeds
049900090107      *
050000090107     c                   movel     'B'           IBEmod
050100090107     c                   z-add     dctaac        IBEaac
050200090107     c                   z-add     dctfil        IBEfil
050300090107     c                   z-add     dctnca        IBEnca
050400090107     c                   z-add     dctaas        IBEaas
050500090107     c                   z-add     dctlnp        IBElnp
050600090107     c                   z-add     dctnrs        IBEnrs
050700090107     c                   z-add     dctnsp        IBEnsp
050800090107     c                   z-add     dctksc        IBEksc
050900090107     c                   movel     dctptr        IBEptr
051000090107     c                   movel     dctflo        IBEflo
051100090107     c                   movel     'P'           IBEtpb
051200090107     c                   If        %subst(knsif:7:1) = 'P'
051300090107     c                   eval      IBEsif = 'P'
051400090107     c                   EndIf
051500090107      *
051600090107     c                   call      'FIDNBER'
051700090107     c                   parm                    fidnbeds
051800090107      *
051900090107     c                   If        obeerr = ' '
052000090107     c                   movel     obeksc        wds_ksc
052100090107      *
052200021025      *
052300090107     c                   Movel     wds_ksc       wPO
052400060628
052500060628     c                   If        Wpo <> *zeros
052600060628     c   07              exsr      VerBenef
052700060628     c                   endif
052800021028      *
052900090107     c                   Movel     OBErag        P1Trsb
053000090107     c                   Movel     OBEvia        P1Tinb
053100090107     c                   Movel     OBEcap        P1Tcab
053200090107     c                   Movel     OBEloc        P1Tlob
053300090107if  4c                   If        OBEnaz = *blanks
053400090108     c                   Movel     OBEprv        P1Tprb
053500090107x   4c                   Else
053600090108     c                   Move      OBEnaz        P1Tprb
053700021028e   1c                   Endif
053800090107     c                   endif
053900021025      *
054000021025     c                   ENDSR
054100021025
054200060628      *****************************************************************
054300060628      *  Verifico il BENEFICIARIO con il P.O. Gestione
054400060628      *****************************************************************
054500060628     c     VerBenef      BEGSR
054600021024
054700060628     c     Wpo           lookup    L6                                     31
054800060628     c                   If        not %found
054900060630     c                   eval      o37err = 'E'
055000060628     c                   exsr      endpgm
055100060628     c                   endif
055200060628      *
055300060628     c                   ENDSR
055400021024      *****************************************************************
055500021025      *  AGGIORNO FNDCT TESTATA C.A.
055600021024      *****************************************************************
055700021025     c     AggDCT        BEGSR
055800060627      *  Se il flag richiesta copia denuncia è uguale a S imposto a blank e imposto
055900060627      *  data stampa
056000060627if  1c                   If        dctrcd = 'S'
056100060627     c                   clear                   dctrcd
056200060627     c                   Z-add     Wdata         dctdic
056300060628     c                   Z-add     i37fgs        dctpor
056400021024     c                   Except    UPDdct
056500021029e   1c                   Endif
056600021024      *
056700021024     c                   ENDSR
056800021024
056900021024      *****************************************************************
057000060627      *  SCRIVO FNDCS - NOTE C.A.
057100021024      *****************************************************************
057200060627     c     WrtDCS        BEGSR
057300021024      *
057400060627
057500060627     c                   clear                   dcspno
057600060627
057700060627     c                   eval      ktpd='C'
057800060627     c                   eval      knkt=dsnumca
057900060627     c                   eval      kstd=' '
058000060627     c                   eval      kdim=Wdata
058100060627     c                   movel     wora          khim
058200060627     c                   movel     dctfca        knks
058300060627     c                   eval      ktrc='N'
058400060627
058500060627      * Cerco l'ultimo progressivo riga relativo alla fase attuale CA se esiste
058600060627     c     kdcsp         setgt     fndcs01l
058700060627     c     kdcse         readpe    fndcs01l
058800060627
058900060627     c                   eval      dcstpd = ktpd
059000060627     c                   eval      dcsnkt = knkt
059100060627     c                   eval      dcsstd = kstd
059200060627     c                   eval      dcsdim = kdim
059300060627     c                   eval      dcshim = khim
059400060627     c                   eval      dcsnks = knks
059500060627     c                   eval      dcstrc = ktrc
059600060627     c                   eval      dcsatb = ' '
059700060627
059800060627     c                   move      knmus         dcspru
059900060627     c                   if        i37fgs <> *zeros
060000060627     c                   movel     i37fgs        dcspos
060100060627     c                   else
060200060627     c                   eval      dcspos = 046
060300060627     c                   endif
060400060627     c                   clear                   dcsstn
060500060627     c                   clear                   dcsft1
060600060627     c                   clear                   dcsdt1
060700060627
060800060627      * Scrivo 2 note xchè su una non ci sto con tutta la frase
060900060627     c                   add       1             dcspno
061000060627     c                   clear                   dcsnot
061100060627     c                   eval      dcsnot = 'Stampata Autodichiarazione'
061200060627     c                   write     fndcs000
061300060627
061400021024     c                   ENDSR
061500021024
061600021024      *****************************************************************
061700021025      *  FINE PROGRAMMA
061800021024      *****************************************************************
061900021024     c     EndPgm        BEGSR
062000021024      *
062100021024      * Chiudo il file stampa utilizzato
062200021025s   1c                   Select
062300060627    1c                   When      I37tde = '1'
062400060627if  2c                   if        %open(FIDN37P1)
062500060627     c                   Close     FIDN37P1
062600021025e   2c                   Endif
062700060627    1c                   When      I37tde = '2'
062800060627if  2c                   if        %open(FIDN37P2)
062900060627     c                   Close     FIDN37P2
063000021025e   2c                   Endif
063100060627    1c                   When      I37tde = '3'
063200060627if  2c                   if        %open(FIDN37P3)
063300060627     c                   Close     FIDN37P3
063400021025e   2c                   Endif
063500021025e   1c                   Endsl
063600021025      *
063700060630     c                   Movel(p)  FIDN37ds      KPJBU
063800021024      *
063900021024     c                   Eval      *inLR = *on
064000060627     c                   return
064100021024      *
064200021024     c                   ENDSR
064300021024      *---------------------------------------------------------------*
064400021024
064500021024     oFNDCT000  e            UPDDCT
064600021024      *                      ------
064700060627     o                       DCTrcd
064800060627     o                       DCTdic
064900060628     o                       DCTpor
065000021024      *---------------------------------------------------------------*
065100021024
065200021024** MSG                                                                       *
065300060627FIDN37R - Tipo Denuncia errato                                                 1
065400060627FIDN37R - C.A. non trovata                                                     2
065500060627                                                                               3
