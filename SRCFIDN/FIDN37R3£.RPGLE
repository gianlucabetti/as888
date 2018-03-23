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
001300060627     fFNDKA01L  if   e           k disk
001400060627     fFNSPE01L  if   e           k disk
001500021024      *
001600021024     fFLNUF01L  uf   e           k disk
001700021024     fFNDCT01L  uf   e           k disk
001800021024      *
001900021024     fFNDCF00F  o    e           k disk
002000021024     fFNDET00F  o    e           k disk
002100021024      *
002200060623     fFIDN37P1  o    e             printer oflind(*in01) usropn
002300021029     f                                     infds(PrtfInf1)
002400060623     fFIDN37P2  o    e             printer oflind(*in02) usropn
002500021029     f                                     infds(PrtfInf2)
002600060623     fFIDN37P3  o    e             printer oflind(*in03) usropn
002700021029     f                                     infds(PrtfInf3)
002800021024
002900021024      *---------------------------------------------------------------*
003000021024
003100021024      *
003200021024      * COSTANTI -----------------------------------------------------*
003300021024      *
003400021024     d K_Mai           c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
003500021024     d K_Min           c                   const('abcdefghijklmnopqrstuvwxyz')
003600021024      *
003700021024      * SCHIERE A TEMPO DI COMPILAZIONE ------------------------------*
003800021024      *
003900021025     d MSG             s             78    dim(3) ctdata perrcd(1)
004000021024      *
004100021024      * SCHIERE A TEMPO DI ESECUZIONE   ------------------------------*
004200021024      *
004300021029     d $DesPM          s                   like(DCSnot) dim(930) inz
004400040804     D L6              S              3  0 DIM(30)                              P.O. GESTITI
004500021024      *
004600021024      * DS DEFINITE ESTERNAMENTE -------------------------------------*
004700021024      *
004800040804      *
004900040804     D TRUL06DS      E DS
005000040804     D  LIN                    1     90  0    DIM(30)                           P.O. COMODO
005100040804      *
005200021024     d KPJBA         e ds
005300060623     d FIDN37DS      e ds
005400021025     d DSTB          e ds                  inz
005500021025     d DDCT01        e ds                  inz
005600021025     d DS_FNARB      e ds                  inz extname(FNARB00F)
005700021025     d TIBS69DS      e ds                  inz
005800021025     d DS_cnaco      e ds                  inz extname(CNACO00F)
005900021025     d DS_cnind      e ds                  inz extname(CNIND00F)
006000021025     d DS_cnclp      e ds                  inz extname(CNCLP00F)
006100021025     d DS_fncls      e ds                  inz extname(FNCLS00F)
006200021025      *
006300021025      * DS DEFINITE INTERNAMENTE -------------------------------------*
006400021029      *
006500021029     d PrtfInf1        ds
006600021029     d  Cur_Line_1           367    368I 0
006700021029     d PrtfInf2        ds
006800021029     d  Cur_Line_2           367    368I 0
006900021029     d PrtfInf3        ds
007000021029     d  Cur_Line_3           367    368I 0
007100021025      *
007200060626     d WLBDAT          ds
007300021025     d  g02dat                 1      8  0
007400021025     d  g02inv                 9     16  0
007500021025     d  g02err                17     17
007600021025     d  g02tgi                18     22  0
007700021025      *
007800021025     d DSnumCA         ds                  inz
007900021025     d  DCTaac
008000021025     d  DCTfil
008100021025     d  DCTnca
008200021024      *
008300021024      * VARIABILI ----------------------------------------------------*
008400021024      *
008500021024     d w0140           s             14  0
008600021024     d wORA            s              4  0
008700021024     d wDATA           s              8  0
008800021024     d wDTGIO          s              8  0
008900021024     d $Err            s              3  0 inz
009000021029     d $Note           s              1    inz(*off)
009100021024     d XX              s              3  0 inz
009200021025     d DATA_eur        s               d   datfmt(*eur)   inz
009300021028     d wCLI            s                   like(ARBksc)   inz
009400060628     d KCLI            s                   like(ARBksc)   inz
009500021028     d w0040           s              4  0 inz
009600060628     d wPO             s              3  0 inz
009700060310     d wCLSflo_7       s              1    inz
009800060627     D Ktpd            S                   LIKE(DCStpd)
009900060627     D Knkt            S                   LIKE(DCSnkt)
010000060627     D Kstd            S                   LIKE(DCSstd)
010100060627     D Kdim            S                   LIKE(DCSdim)
010200060627     D Khim            S                   LIKE(DCShim)
010300060627     D Knks            S                   LIKE(DCSnks)
010400060627     D Ktrc            S                   LIKE(DCStrc)
010500060628     d w§DCTport       s                   like(§DCTport) inz
010600021024
010700060627
010800060627     d prgnot          s                   like(dcspno)   inz(999)              progressivo note
010900021024      *---------------------------------------------------------------*
011000021024
011100021024      *****************************************************************
011200021024      *  RIEPILOGO INDICATORI
011300021024      *****************************************************************
011400021029      * 01-04 - OverFlow
011500021025      * 30    - Generico
011600021025      * 41-43 - Condizionano la stampa della merce mancante nelle 3
011700021025      *         diverse unità di misura: Colli, Pezzi e Kg.
011800060630      * 44    - Condiziono la stampa della descrizione solo per mancanze
011900060630      *****************************************************************
012000060626
012100060626      * Definizioni Chiavi
012200060626
012300060626     c     K03DCT01      Klist
012400060626     c                   Kfld                    I37aac
012500060626     c                   Kfld                    I37fil
012600060626     c                   Kfld                    I37nca
012700060626      *
012800060626     c     K04BLP01      Klist
012900060626     c                   Kfld                    DCTaas
013000060626     c                   Kfld                    DCTlnp
013100060626     c                   Kfld                    DCTnrs
013200060626     c                   Kfld                    DCTnsp
013300060628      *
013400060628     c     K05AR601      klist
013500060628     c                   Kfld                    DCTaas
013600060628     c                   Kfld                    DCTlnp
013700060628     c                   Kfld                    DCTnrs
013800060628     c                   Kfld                    DCTnsp
013900060628     c                   kfld                    AR6trc
014000060626      *
014100060626     c     K03DCS01      Klist
014200060626     c                   Kfld                    DCStpd
014300060626     c                   Kfld                    DCSnkt
014400060626     c                   Kfld                    DCSstd
014500060626      *
014600060626     c     K04DKA01      Klist
014700060626     c                   Kfld                    I37aac
014800060626     c                   Kfld                    I37fil
014900060626     c                   Kfld                    I37nca
015000060626     c                   Kfld                    DKAtrc
015100060626      *
015200060626     c     K03SPE01      Klist
015300060626     c                   Kfld                    SPEfls
015400060626     c                   Kfld                    SPEcli
015500060626     c                   Kfld                    SPEcod
015600060628     c                   eval      SPEfls = 'L'
015700060628     c                   eval      SPEcod = '006'
015800060626      *
015900060627     C     KDCSE         klist
016000060627     C                   KFLD                    KTPD
016100060627     C                   KFLD                    KNKT
016200060627     C                   KFLD                    KSTD
016300060627     C                   KFLD                    KDIM
016400060627     C                   KFLD                    KHIM
016500060627     C                   KFLD                    KNKS
016600060627     C                   KFLD                    KTRC
016700060627
016800060627     c     kdcsp         KLIST
016900060627     c                   KFLD                    ktpd
017000060627     c                   KFLD                    knkt
017100060627     c                   KFLD                    kstd
017200060627     c                   KFLD                    kdim
017300060627     c                   KFLD                    khim
017400060627     c                   KFLD                    knks
017500060627     c                   KFLD                    ktrc
017600060627     c                   KFLD                    prgnot
017700060627      *
017800060628      * Apro il file stampa interessato
017900060628s   1c                   Select
018000060628    1c                   When      I37tde = '1'
018100060628if  2c                   if        not %open(FIDN37P1)
018200060628     c                   Open      FIDN37P1
018300060628e   2c                   endif
018400060628     c                   Clear                   DN37P1T
018500060628    1c                   When      I37tde = '2'
018600060628if  2c                   if        not %open(FIDN37P2)
018700060628     c                   Open      FIDN37P2
018800060628e   2c                   endif
018900060628     c                   Clear                   DN37P2T
019000060628    1c                   When      I37tde = '3'
019100060628if  2c                   if        not %open(FIDN37P3)
019200060628     c                   Open      FIDN37P3
019300060628e   2c                   endif
019400060628     c                   Clear                   DN37P3T
019500060628x   1c                   Other
019600060628     c                   Eval      $Err = 1
019700060628     c                   Exsr      EndPgm
019800060628e   1c                   Endsl
019900060627      *
020000060627      *
020100060627     C                   EXSR      CARL6
020200021024
020300021024      * Aggancio FNDCT per dati C.A. (da aggiornare)
020400021024     c     K03DCT01      Chain     FNDCT000
020500021025if  1c                   If        not %found(FNDCT01L)
020600021024     c                   Eval      $Err = 2
020700021024     c                   Exsr      EndPgm
020800021025e   1c                   Endif
020900060320
021000060320      * Reperisco dati Spedizione
021100060320      *
021200060320      * se CA con legami e se sono in arrivo rispetto la CA  recupero numero spedizione da FNDCK
021300060320     c                   movel     dctflo        ddct01
021400060320     c                   if        §dctlega = 'S'
021500060320     c     dctlna        lookup    L6                                     31
021600060320     c                   if        *in31
021700060320     c     K03dct01      chain     fndck01l
021800060320     c                   if        %found(fndck01l)
021900060320     c                   eval      dctaas = dckaas
022000060320     c                   eval      dctlnp = dcklnp
022100060320     c                   eval      dctnrs = dcknrs
022200060320     c                   eval      dctnsp = dcknsp
022300060320     c                   endif
022400060320     c                   endif
022500060320     c                   endif
022600060320      *
022700060320     c                   clear                   DS_FNARB
022800060320     c     K04BLP01      chain     FNBLP000                           30
022900060320if  1c                   if        not %found(FNBLP01L)
023000060320     c     K04BLP01      chain     FNARB000                           30
023100060320e   1c                   endif
023200060710      * non trovata bolla in filiale favo a fine programma
023300060710     c                   If        *in30
023400060710     c                   Exsr      EndPgm
023500060710e   1c                   Endif
023600060320
023700060320      * Reperisco i Dati del Beneficiario
023800060320     c                   Exsr      RepBenef
023900021024
024000021024      * Reperisco dati Punto Operativo
024100060623     c     I37fgs        Chain     AZORG
024200021025if  1c                   If            %found(AZORG01L)
024300021024     c                             and ORGfva = *blanks
024400021024     c                   Exsr      RieDatiPO
024500021025e   1c                   Endif
024600021024
024700021024      * Preparo campi di output
024800021029     c                   Exsr      RieVarLet
024900060628
025000021024
025100021024      * Stampo testo denuncia tipo lettera
025200021025s   1c                   Select
025300060623    1c                   When      I37tde = '1'
025400060626     c                   Write     DN37P1I
025500060626     c                   Write     DN37P1T
025600060623    1c                   When      I37tde = '2'
025700060626     c                   Write     DN37P2I
025800060626     c                   Write     DN37P2T
025900060623    1c                   When      I37tde = '3'
026000060626     c                   write     DN37P3I
026100060626     c                   Write     DN37P3T
026200021025e   1c                   Endsl
026300021029
026400021029      * Stampo note e chiusura lettera
026500021029      * - Note
026600021029     c                   Eval      $Note  =  *on
026700021029     c                   Clear                   xx
026800021029do  2c                   Dow       Xx <= %elem($DesPM) - 3
026900021029     c                   Exsr      PrtNOTE
027000021029e   2c                   Enddo
027100021029      * - Chiusura
027200021029     c                   Eval      $Note  =  *off
027300021029     c                   Exsr      Test_OF
027400021029s   2c                   Select
027500060623    2c                   When      I37tde = '1'
027600060626     c                   Write     DN37P1F
027700060623    2c                   When      I37tde = '2'
027800060626     c                   Write     DN37P2F
027900060626    2c                   When      I37tde = '3'
028000060626     c                   Write     DN37P3F
028100021029e   2c                   Endsl
028200021024
028300021024      * Aggiorno files
028400021025     c                   Exsr      AggDCT
028500060623     c                   Exsr      WrtDCS
028600021024
028700021024      * Fine
028800021024     c                   Exsr      EndPgm
028900021024
029000021024      *****************************************************************
029100021025      *  ROUTINE INIZIALE
029200021024      *****************************************************************
029300021024     c     *InzSR        BEGSR
029400021024      *
029500021024     c     *Entry        Plist
029600021024     c                   Parm                    KPJBA
029700021024      *
029800060623     c                   Movel     KPJBU         FIDN37ds
029900060627     c                   Clear                   O37err
030000060627     c                   Clear                   O37msg
030100060628      * verifico se tipo madalità  devo controllare il beneficiario
030200060628     c                   eval      *in07 = (i37mod = 'G')
030300021025      *
030400021024      * Reperisco Data e Ora
030500021024     c                   Time                    W0140
030600021024     c                   Movel     W0140         Wora
030700021024     c                   Move      W0140         Wdtgio
030800021024     c                   Z-add     Wdtgio        G02dat
030900021024     c                   Clear                   G02err
031000021024     c                   Call      'XSRDA8'
031100021024     c                   Parm                    WLBDAT
031200021024     c                   Movel     G02inv        Wdata
031300040804      *
031400021024     c                   ENDSR
031500040804      *****************************************************************
031600040804      * CARICO SCHIERA L6 DA TABELLA £6
031700040804      *****************************************************************
031800040804     C     carl6         BEGSR
031900040804      *
032000040804     C                   CLEAR                   L6
032100040804     C                   CLEAR                   TRUL06DS
032200040804     C                   MOVE      '£6'          D06COD
032300060623     C                   MOVEL     i37fgs        D06key
032400040804     C                   MOVEL(p)  TRUL06DS      KPJBU
032500040804      *
032600040804     C                   CALL      'TRUL06R'
032700040804     C                   PARM                    KPJBA
032800040804      *
032900040804     C                   MOVEL     KPJBU         TRUL06DS
033000040804     C                   MOVEA     LIN           L6
033100040804      *
033200040804     c                   endsr
033300021024
033400021024      *****************************************************************
033500021025      *  IMPOSTO DATI PUNTO OPERATIVO
033600021024      *****************************************************************
033700021024     c     RieDatiPO     BEGSR
033800021024      *
033900021028     c                   Movel     ORGdes        P1Tpof
034000060627     c                   Movel     ORGloc        P1Tloc1
034100021024      *
034200030320if  1c                   If        P1Tloc1 <> *blanks
034300030320     c                   Eval      P1Tloc1 = %trim(P1Tloc1) + ', lì '
034400060626     c                                     + %editc(Wdtgio:'Y')
034500030320e   1c                   Endif
034600021024      *
034700021024     c                   ENDSR
034800021024
034900021024      *****************************************************************
035000021025      *  IMPOSTO VARIABILI TESTO DENUNCIA  ** LETTERA **
035100021024      *****************************************************************
035200021029     c     RieVarLet     BEGSR
035300021025      *
035400021025      * Dati C.A.
035500021029     c                   Movel(p)  DCTfil        P1Tnca
035600021029     c                   Eval      P1Tnca = %trim(P1Tnca) + '-'
035700021029     c                                    + %trim(%editc(DCTnca:'Z'))
035800021028     c                   Z-add     DCTmgc        G02inv
035900021028     c                   Movel     DCTaac        G02inv
036000021028     c                   Movel     '3'           G02err
036100021028     c                   Call      'XSRDA8'
036200021028     c                   Parm                    WLBDAT
036300021028     c                   Z-add     G02dat        P1Tdca
036400021025     c                   Movel     DCTflo        DDCT01
036500060628     c                   movel     P1tnca        P1Tnca2
036600021025     c                   Z-add     DCTaac        P1Taac
036700021025     c                   Z-add     DCTmgc        G02inv
036800021025     c                   Movel     DCTaac        G02inv
036900021025     c                   Movel     '3'           G02err
037000021025     c                   Call      'XSRDA8'
037100021025     c                   Parm                    WLBDAT
037200021025     c                   Movel     G02dat        P1Tmgc
037300021025      *
037400021025      *
037500021025     c                   Z-add     DCTncn        P1Tncn
037600021025     c                   Z-add     DCTncn        P1Tncn2
037700021025     c                   Z-add     DCTpkd        P1Tpkd2
037800021025      *
037900021025      * Dati Spedizione
038000021025     c                   Z-add     ARBlnp        P1Tlnp
038100021025     c                   Z-add     ARBnrs        P1Tnrs
038200021025     c                   Z-add     ARBnsp        P1Tnsp
038300021025     c                   Z-add     ARBmgs        G02inv
038400021025     c                   Movel     ARBaas        G02inv
038500021025     c                   Movel     '3'           G02err
038600021025     c                   Call      'XSRDA8'
038700021025     c                   Parm                    WLBDAT
038800021025     c                   Z-add     G02dat        P1Tdsp
038900021025     c                   Z-add     ARBncl        P1Tncl
039000021025     c                   Z-add     ARBpkf        P1Tpkf
039100021025     c                   Z-add     ARBrmn        P1Trmn
039200021025     c                   Movel     ARBrsd        P1Trsd
039300021025      *
039400021025      * Merce Mancante
039500060630     c                   Movea     '0000'        *in(41)
039600021025     c                   if        DCTncn <> 0
039700021025     c                   Z-add     DCTncn        P1Tncn
039800021025     c                   Movel     *on           *in41
039900021025     c                   Endif
040000021025     c                   if        DCTnpz <> 0
040100021025     c                   Z-add     DCTnpz        P1Tnpz
040200021025     c                   Movel     *on           *in42
040300021025     c                   Endif
040400021025     c                   if        DCTpkd <> 0
040500021025     c                   Z-add     DCTpkd        P1Tpkd
040600021025     c                   Movel     *on           *in43
040700021025     c                   Endif
040800021025      *
040900021029      * SCHIERA Descrizione pezzi mancanti/danneggiati
041000021025     c                   Clear                   XX
041100021025     c                   Clear                   $DesPM
041200021025if  1c                   If        DCTnpz  > *zeros
041300021025     c                   Movel     'C'           DCStpd
041400021025     c                   Movel(p)  DSnumCA       DCSnkt
041500021025     c                   Movel     'P'           DCSstd
041600021025     c     K03DCS01      Chain     FNDCS000                           30
041700021025do  2c                   Dow       Not *in30  and  Xx < %elem($DesPM)
041800021025     c                   Add       1             xx
041900021025     c                   Movel     DCSnot        $DesPM(xx)
042000021025     c     K03DCS01      Reade     FNDCS000                               30
042100021025e   2c                   Enddo
042200021025e   1c                   Endif
042300060630      * se dichiarazione di smarrimento "3"  stampo la descrizione solo se c'è
042400060630      *
042500060630     c                   If        i37tde = '3' and xx > 0
042600060630     c                   eval      *in(44) = *on
042700060627     c                   endif
042800021024      *
042900021024     c                   ENDSR
043000021029
043100021029      *****************************************************************
043200021029      *  IMPOSTO NOTE IN STAMPA  &  STAMPO NOTE
043300021029      *****************************************************************
043400021029     c     PrtNote       BEGSR
043500021029      *
043600021029     c                   Add       1             xx
043700021029     c                   Movel     $DesPM(xx)    P1Tpdd1
043800021029     c                   Add       1             xx
043900021029     c                   Movel     $DesPM(xx)    P1Tpdd2
044000021029     c                   Add       1             xx
044100021029     c                   Movel     $DesPM(xx)    P1Tpdd3
044200021029      *
044300021029if  1c                   If           P1Tpdd1 <> *blanks
044400021029     c                             or P1Tpdd2 <> *blanks
044500021029     c                             or P1Tpdd3 <> *blanks
044600021029     c                   Exsr      Test_OF
044700021029s   2c                   Select
044800060626    2c                   When      I37tde = '1'
044900060626     c                   Write     DN37P1N
045000060626    2c                   When      I37tde = '2'
045100060626     c                   Write     DN37P2N
045200060626    2c                   When      I37tde = '3'
045300060626     c                   Write     DN37P3N
045400021029e   2c                   Endsl
045500021029e   1c                   Endif
045600021029      *
045700021029     c                   ENDSR
045800021029
045900021029      *****************************************************************
046000021029      *  CONTROLLO OVERFLOW
046100021029      *****************************************************************
046200021029     c     Test_OF       BEGSR
046300021029      *
046400021029s   1c                   Select
046500021029      *
046600060626    1c                   When      I37tde = '1'
046700021029if  2c                   If        $Note = *on
046800021030     c                   eval      *in01 = (Cur_Line_1 >= 61)
046900021029x   2c                   Else
047000021030     c                   eval      *in01 = (Cur_Line_1 >= 51)
047100021029e   2c                   Endif
047200021029if  2c                   If        *in01
047300060626     c                   Write     DN37P1I
047400021029     c                   Eval      *in01 = *off
047500021029e   2c                   Endif
047600021029      *
047700060626    1c                   When      I37tde = '2'
047800021029if  2c                   If        $Note = *on
047900021030     c                   eval      *in02 = (Cur_Line_2 >= 61)
048000021029x   2c                   Else
048100021030     c                   eval      *in02 = (Cur_Line_2 >= 51)
048200021029e   2c                   Endif
048300021029if  2c                   If        *in02
048400060626     c                   Write     DN37P2I
048500021029     c                   Eval      *in02 = *off
048600021029e   2c                   Endif
048700021029      *
048800060626    1c                   When      I37tde = '3'
048900021029if  2c                   If        $Note = *on
049000060627     c                   eval      *in03 = (Cur_Line_3 >= 61)
049100021029x   2c                   Else
049200060627     c                   eval      *in03 = (Cur_Line_3 >= 51)
049300021029e   2c                   Endif
049400060627if  2c                   If        *in03
049500060626     c                   Write     DN37P3I
049600060627     c                   Eval      *in03 = *off
049700021029e   2c                   Endif
049800021029      *
049900021029e   1c                   Endsl
050000021029      *
050100021029     c                   ENDSR
050200021025
050300021025      *****************************************************************
050400021025      *  IMPOSTO DATI CLIENTE BENEFICIARIO PER STAMPA
050500021025      *  AUTO-DICHIARAZIONE DEI P.O. DI SMARRIMENTO DELLA MERCE
050600021025      *****************************************************************
050700021025     c     RepBENEF      BEGSR
050800021025      *
050900021025      *
051000060310     c                   clear                   wCLSflo_7
051100021025     c                   Clear                   wCli
051200021028     c                   Move      DCTksc        w0040
051300060628     c                   Movel     DCTksc        wPO
051400060628
051500060628     c                   If        Wpo <> *zeros
051600060628     c   07              exsr      VerBenef
051700060628     c                   endif
051800021028      *
051900021028if  1c                   If        DCTksc > *zeros
052000060628
052100021028      *
052200021028      * Se il Beneficiario alternativo è generico aggancio FNDKA
052300021028      *  altrimenti CNIND
052400021028if  2c                   If        w0040 = 8888
052500021028     c                   Movel     'C'           DKAtrc
052600021028     c     K04DKA01      Chain     FNDKA000
052700021028if  3c                   If        %found(FNDKA01L)
052800021028     c                   Movel     DKArag        P1Trsb
052900021028     c                   Movel     DKAvia        P1Tinb
053000021028     c                   Movel     DKAcap        P1Tcab
053100021028     c                   Movel     DKAloc        P1Tlob
053200021028if  4c                   If        DKAnaz = *blanks
053300021029     c                   Movel     DKAprv        P1Tlob
053400021028x   4c                   Else
053500021028     c                   Move      DKAnaz        P1Tlob
053600021028e   4c                   Endif
053700021028e   3c                   Endif
053800021028x   2c                   Else
053900021028     c                   Clear                   TIBS69DS
054000021028     c                   Z-add     DCTksc        I69kac
054100021028     c                   Z-add     DCTksc        I69kin
054200060310     c                   z-add     DCTksc        I69kcs
054300021028     c                   Exsr      Ctr_ANAGRA
054400021028     c                   Movel     ACOrag        P1Trsb
054500021028     c                   Movel     INDvia        P1Tinb
054600021028     c                   Movel     INDcae        P1Tcab
054700021029     c                   Movel     INDcit        P1Tlob
054800021028if  3c                   If        INDsta = *blanks
054900021029     c                   Movel     INDprv        P1Tprb
055000021028x   3c                   Else
055100021029     c                   Move      INDsta        P1Tprb
055200021028e   3c                   Endif
055300060628      * Controllo se esiste il luogo per il beneficiario alternativo
055400060628     c                   eval      specli = dctksc
055500060628     c     K03SPE01      chain     FNSPE000
055600060628      *   se esiste in FNSPE
055700060628if  3c                   IF        %found(FNSPE01L)
055800060628     c                   movel     SPErag        P1Trsb
055900060628     c                   movel     SPEind        P1Tinb
056000060628     c                   movel     SPEcap        P1Tcab
056100060628     c                   movel     SPEloc        P1Tlob
056200060628if  4c                   If        SPEnaz = *blanks
056300060628     c                   Movel     Spepro        P1Tprb
056400060628x   4c                   Else
056500060628     c                   Move      SPEnaz        P1Tprb
056600060628e   4c                   Endif
056700060628e   3c                   Endif
056800060310      * - Imposto se devo stampare in Inglese
056900060628if  3c                   if        %subst(CLSflo:7:1) <> *blanks
057000060310     c                   eval      wCLSflo_7 = %subst(CLSflo:7:1)
057100060628e   3c                   endif
057200060628e   2c                   Endif
057300021028      *
057400021028x   1c                   ELSE
057500021028      *
057600060628     c                   exsr      Rep_CodCli
057700060628     c                   If        Wpo <> *zeros
057800060628     c   07              exsr      VerBenef
057900060628     c                   endif
058000060628      *
058100060628if  2c                   if             Kcli > *zeros
058200060628     c                             and  w0040 <> 8888
058300060628     c                             and  w0040 <> 9999
058400060628     c                   eval      specli = kcli
058500060628     c     K03SPE01      chain     FNSPE000                           30
058600060628x   2c                   else
058700060628     c                   eval      *in30    = *on
058800060628e   2c                   endif
058900060628      *
059000060628      * Muovo campi per stampa
059100060628if  2c                   IF        NOT *in30
059200060628      *
059300060628     c                   movel     SPErag        P1Trsb
059400060628     c                   movel     SPEind        P1Tinb
059500060628     c                   movel     SPEcap        P1Tcab
059600060628     c                   movel     SPEloc        P1Tlob
059700060628if  3c                   If        SPEnaz = *blanks
059800060628     c                   Movel     Spepro        P1Tprb
059900060628x   3c                   Else
060000060628     c                   Move      SPEnaz        P1Tprb
060100060628e   3c                   Endif
060200060628      *
060300060628x   2c                   Else
060400060628      *
060500060628if  3c                   If        DCTptr = 'D'
060600021028     c                   Movel     ARBrsd        P1Trsb
060700021028     c                   Movel     ARBind        P1Tinb
060800021028     c                   Movel     ARBcad        P1Tcab
060900021028     c                   Movel     ARBlod        P1Tlob
061000060628if  4c                   If        ARBnzd = *blanks
061100021029     c                   Movel     ARBprd        P1Tprb
061200060628x   4c                   Else
061300021029     c                   Move      ARBnzd        P1Tprb
061400060628e   4c                   Endif
061500060628x   3c                   Else
061600021028     c                   Movel     ARBrsm        P1Trsb
061700021028     c                   Movel     ARBinm        P1Tinb
061800021028     c                   Movel     ARBcam        P1Tcab
061900021028     c                   Movel     ARBlom        P1Tlob
062000060628if  4c                   If        ARBnzm = *blanks
062100021029     c                   Movel     ARBprm        P1Tprb
062200060628x   4c                   Else
062300021029     c                   Move      ARBnzm        P1Tprb
062400060628e   4c                   Endif
062500060628e   3c                   Endif
062600060628      *
062700060628e   2c                   Endif
062800021028      *
062900021028e   1c                   Endif
063000021025      *
063100021025     c                   ENDSR
063200021025
063300021025      *****************************************************************
063400021025      *  AGGANCIO ANAGRAFICO
063500021025      *****************************************************************
063600021025     c     Ctr_ANAGRA    BEGSR
063700021025      *
063800021025     c                   Clear                   DS_cnaco
063900021025     c                   Clear                   DS_cnind
064000021025     c                   Clear                   DS_cnclp
064100021025     c                   Clear                   DS_fncls
064200021025      *
064300021025     c                   Call      'TIBS69R'
064400021025     c                   Parm                    TIBS69DS
064500021025     c                   Parm                    DS_cnaco
064600021025     c                   Parm                    DS_cnind
064700021025     c                   Parm                    DS_cnclp
064800021025     c                   Parm                    DS_fncls
064900021025      *
065000021025     c                   ENDSR
065100060628
065200060628      *---------------------------------------------------------------*
065300060628      *?Reperimento Codice Cliente                                   ?*
065400060628      *---------------------------------------------------------------*
065500060628     c     Rep_CodCli    BEGSR
065600060628      *
065700060628      * Se "Mamma di cambio porto" per ottenere il tipo porto della
065800060628      *   spedizione inverto quello della C.A.
065900060628sel 1c                   select
066000060628w   1c                   when      ARBcca   <> '9'
066100060628     c                   eval      w§DCTport = §DCTport
066200060628w   1c                   when      §DCTport  = 'A'
066300060628     c                   eval      w§DCTport = 'F'
066400060628w   1c                   when      §DCTport  = 'F'
066500060628     c                   eval      w§DCTport = 'A'
066600060628e   1c                   endsl
066700060628      *
066800060628      *  Se assegnato in partenza verifico il codice destinatario
066900060628     c                   clear                   AR6ksc
067000060628if  1c                   If        w§DCTport = 'A'
067100060628     c                   eval      AR6trc = '1'
067200060628     c     K05AR601      chain     FIAR6000
067300060628e   1c                   endif
067400060628      *
067500060628      * Determino il codice cliente da utilizzare per verifica luogo
067600060628      *   6 per danni
067700060628sel 1c                   select
067800060628      * - destinatario PA
067900060628w   1c                   when           DCTptr    = 'D'
068000060628     c                             and  w§DCTport = 'A'
068100060628if  2c                   if        AR6ksc <> *zeros
068200060628     c                   movel     AR6ksc        Kcli
068300060628x   2c                   else
068400060628     c                   z-add     ARBksc        Kcli
068500060628e   2c                   endif
068600060628      * - mittente PF
068700060628w   1c                   when           DCTptr    = 'M'
068800060628     c                             and  w§DCTport = 'F'
068900060628     c                   z-add     ARBksc        Kcli
069000060628      * - mittente PA
069100060628w   1c                   when           DCTptr    = 'M'
069200060628     c                             and  w§DCTport = 'A'
069300060628     c                   z-add     ARBccm        Kcli
069400060628      *
069500060628     c                   endsl
069600060628
069700060628     c                   movel     kcli          wpo
069800060628
069900060628     c                   If        wpo = *zeros
070000060628     c                   If        DCTptr = 'D'
070100060628     c                   z-add     DCTlna        wpo
070200060628     c                   else
070300060628     c                   movel     §dctlnpc      wpo
070400060628     c                   endif
070500060628     c                   endif
070600060628      *
070700060628     c                   ENDSR
070800060628
070900060628      *****************************************************************
071000060628      *  Verifico il BENEFICIARIO con il P.O. Gestione
071100060628      *****************************************************************
071200060628     c     VerBenef      BEGSR
071300021024
071400060628     c     Wpo           lookup    L6                                     31
071500060628     c                   If        not %found
071600060630     c                   eval      o37err = 'E'
071700060628     c                   exsr      endpgm
071800060628     c                   endif
071900060628      *
072000060628     c                   ENDSR
072100021024      *****************************************************************
072200021025      *  AGGIORNO FNDCT TESTATA C.A.
072300021024      *****************************************************************
072400021025     c     AggDCT        BEGSR
072500060627      *  Se il flag richiesta copia denuncia è uguale a S imposto a blank e imposto
072600060627      *  data stampa
072700060627if  1c                   If        dctrcd = 'S'
072800060627     c                   clear                   dctrcd
072900060627     c                   Z-add     Wdata         dctdic
073000060628     c                   Z-add     i37fgs        dctpor
073100021024     c                   Except    UPDdct
073200021029e   1c                   Endif
073300021024      *
073400021024     c                   ENDSR
073500021024
073600021024      *****************************************************************
073700060627      *  SCRIVO FNDCS - NOTE C.A.
073800021024      *****************************************************************
073900060627     c     WrtDCS        BEGSR
074000021024      *
074100060627
074200060627     c                   clear                   dcspno
074300060627
074400060627     c                   eval      ktpd='C'
074500060627     c                   eval      knkt=dsnumca
074600060627     c                   eval      kstd=' '
074700060627     c                   eval      kdim=Wdata
074800060627     c                   movel     wora          khim
074900060627     c                   movel     dctfca        knks
075000060627     c                   eval      ktrc='N'
075100060627
075200060627      * Cerco l'ultimo progressivo riga relativo alla fase attuale CA se esiste
075300060627     c     kdcsp         setgt     fndcs01l
075400060627     c     kdcse         readpe    fndcs01l
075500060627
075600060627     c                   eval      dcstpd = ktpd
075700060627     c                   eval      dcsnkt = knkt
075800060627     c                   eval      dcsstd = kstd
075900060627     c                   eval      dcsdim = kdim
076000060627     c                   eval      dcshim = khim
076100060627     c                   eval      dcsnks = knks
076200060627     c                   eval      dcstrc = ktrc
076300060627     c                   eval      dcsatb = ' '
076400060627
076500060627     c                   move      knmus         dcspru
076600060627     c                   if        i37fgs <> *zeros
076700060627     c                   movel     i37fgs        dcspos
076800060627     c                   else
076900060627     c                   eval      dcspos = 046
077000060627     c                   endif
077100060627     c                   clear                   dcsstn
077200060627     c                   clear                   dcsft1
077300060627     c                   clear                   dcsdt1
077400060627
077500060627      * Scrivo 2 note xchè su una non ci sto con tutta la frase
077600060627     c                   add       1             dcspno
077700060627     c                   clear                   dcsnot
077800060627     c                   eval      dcsnot = 'Stampata Autodichiarazione'
077900060627     c                   write     fndcs000
078000060627
078100021024     c                   ENDSR
078200021024
078300021024      *****************************************************************
078400021025      *  FINE PROGRAMMA
078500021024      *****************************************************************
078600021024     c     EndPgm        BEGSR
078700021024      *
078800021024      * Chiudo il file stampa utilizzato
078900021025s   1c                   Select
079000060627    1c                   When      I37tde = '1'
079100060627if  2c                   if        %open(FIDN37P1)
079200060627     c                   Close     FIDN37P1
079300021025e   2c                   Endif
079400060627    1c                   When      I37tde = '2'
079500060627if  2c                   if        %open(FIDN37P2)
079600060627     c                   Close     FIDN37P2
079700021025e   2c                   Endif
079800060627    1c                   When      I37tde = '3'
079900060627if  2c                   if        %open(FIDN37P3)
080000060627     c                   Close     FIDN37P3
080100021025e   2c                   Endif
080200021025e   1c                   Endsl
080300021025      *
080400060630     c                   Movel(p)  FIDN37ds      KPJBU
080500021024      *
080600021024     c                   Eval      *inLR = *on
080700060627     c                   return
080800021024      *
080900021024     c                   ENDSR
081000021024      *---------------------------------------------------------------*
081100021024
081200021024     oFNDCT000  e            UPDDCT
081300021024      *                      ------
081400060627     o                       DCTrcd
081500060627     o                       DCTdic
081600060628     o                       DCTpor
081700021024      *---------------------------------------------------------------*
081800021024
081900021024** MSG                                                                       *
082000060627FIDN37R - Tipo Denuncia errato                                                 1
082100060627FIDN37R - C.A. non trovata                                                     2
082200060627                                                                               3
