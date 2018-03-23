000100081013      *PARMS CLOSQLCSR(*ENDMOD) DBGVIEW(*SOURCE) DYNUSRPRF(*OWNER)
000200081013      *PARMS COMMIT(*NONE) OPTION(*NOXREF)
000300011211      *---------*
000400011211      * FNLS13R *-----------------------------------------------------*
000500011211      *---------*                                                     *
000600011211      *             ELENCO INVERSIONI DISK C SU VDL       - STAMPA -  *
000700001122      *---------------------------------------------------------------*
000800001122
000900040317     H DECEDIT('0,') DATEDIT(*DMY/) option(*nodebugio)
001000011211
001100011211      *****************************************************************
001200011211      *                                                               *
001300011211      *   A R C H I V I                                               *
001400011211      *                                                               *
001500011211      *****************************************************************
001600011211
001700011211     FFLTR704L  IF   E           K DISK
001800011211     FFNBLT27L  IF   E           K DISK
001900170131     FFiars03L  IF   E           K DISK
002000020103     FFNBLP01L  IF   E           K DISK
002100020205     FAZORG01L  IF   E           K DISK
002200020102      *
002300081030     FFNLS13P   O    E             PRINTER OFLIND(*in31)
002400020212
002500020212      *****************************************************************
002600020212      *                                                               *
002700020212      *   S C H I E R E                                               *
002800020212      *                                                               *
002900020212      *****************************************************************
003000020212
003100020212     D $PO             S                   like(ORGFIL)  dim(500) inz
003200020212     D $POD            S                   like(AZORGDS) dim(500) inz
003300051014     D $FiarS          S                   like(FiarSDS) dim(30)  inz
003400081013     d $FiarS2         s                   like(FiarSDS) dim(30)  inz
003500011211
003600011211      *****************************************************************
003700011211      *                                                               *
003800011211      *   D S   E S T E R N E / I N T E R N E                         *
003900011211      *                                                               *
004000011211      *****************************************************************
004100011211
004200081014     d Status         sds
004300081014     d  SDSpgm           *proc
004400081014      *
004500011211     D KPJBA         e ds
004600011211      *
004700011211      * Parametri
004800011211     D FNLS13DS        ds                  inz
004900011211     D  D13DDA                 1      8  0
005000011211     D  D13DAL                 9     16  0
005100020624     D  D13POG                17     19  0
005200011211      *
005300040317      * - Parametri x Controllo profilo utenti
005400040317     d TIBS34DS      e ds                  inz
005500040317      * - Ds di riferimento al file esterno AZUTE00F
005600040317     d AZUTEds       e ds                  extname(AZUTE00F)
005700040317      * - Ds per dati organigramma
005800040317     d DDatiUte      e ds
005900011211      *
006000040317      * Ridefinizione dei trk per avere la possibilità di:
006100040317      * - fare un'unica CLEAR comulativa (non campo per campo), senza
006200040317      *   avere tali archivi in aggiornamento, ma solo in lettura
006300040317      * - valorizzare tutti i campi del trk con un'unica MOVEL
006400040317     D dsFNBLT       e ds                  extname(FNBLT00F) inz
006500040317     D dsFNBLT1      e ds                  extname(FNBLT00F) inz prefix(P)
006600040317     D dsFNBLT2      e ds                  extname(FNBLT00F) inz prefix(A)
006700081013       //
006800081013       // Dati delle altre letture dello stesso "Chi Sono"
006900081013     d ds_FLTR7      e ds                  extname(FLTR700F) inz prefix(X)
007000020212      *
007100020212      * Ridefinizione dei campi per avere la possibilità di fare una
007200020212      *  unica MOVEL comulativa (non campo per campo), senza avere
007300020212      *  tale archivio in aggiornamento, ma solo in lettura
007400040317     D AZORGds         ds                  inz
007500020212     D  ORGdit
007600020212     D  ORGfva
007700020212     D  ORGfag
007800020212     D  ORGcts
007900020212     D  ORGcaa
008000020212     D  ORGfl1
008100020212     D  ORGfl2
008200020212     D  ORGfl3
008300020212     D  ORGdes
008400020212     D  ORGfel
008500040317      *
008600040317      * Ridefinizione dei campi per facilitare il salvataggio di solo
008700040317      *  questi campi con un'unica MOVEL in schiera (non campo per campo)
008800051014     D FiarSds         ds                  inz
008900051014     D  arSfls
009000051014     D  arSlna
009100051014     D  arSnrs
009200051014     D  arSnsc
009300011211      *
009400011211      *
009500020103     D wPD1            ds                  inz
009600011211     D  LNP                    1      3
009700011211     D  LNA                    4      6
009800011211     D  NRS                    7      8
009900011211     D  NSC                    9     15
010000011211     D  ZNC                   16     17
010100011211     D  BAR                    1     17
010200011211     D  CHK                   18     18
010300011211     D  CHR                   19     19
010400020218     D wPD3            ds                  inz
010500020218     D  LNP3                   1      3
010600020218     D  LNA3                   4      6
010700020218     D  NRS3                   7      8
010800020218     D  NSC3                   9     15
010900020218     D  ZNC3                  16     17
011000020218     D  BAR3                   1     17
011100011211      *
011200011211      * Struttura del campo TR7TIM
011300011211     D                 ds
011400011211     D  TR7AMG                 1      8  0 inz
011500011211     D  TR7HMS                 9     14  0 inz
011600011211     D TR7TIM                  1     14
011700011211      *
011800011211      * Parametri per controllo ed inversione data
011900011211     D WLBDAT          ds                  inz
012000011211     D  G02DAT                 1      8  0
012100011211     D  G02INV                 9     16  0
012200011211     D  G02ERR                17     17    inz('3')
012300011211     D  G02TGI                18     22  0
012400011211
012500011211      *****************************************************************
012600011211      *                                                               *
012700011211      *   V A R I A B I L I                                           *
012800011211      *                                                               *
012900011211      *****************************************************************
013000011211
013100081013     d StringaSQL      s            400    inz
013200011211     D A29             S             29    inz
013300020103     D A08dx           S              8    inz
013400020103     D A18dx           S             18    inz
013500020103     D wPDx            S             30    inz
013600020103     D wSCBartol       S                   like(TR7pd1) inz
013700020103     D wChiSono        S                   like(TR7pd2) inz
013800040317      *
013900020102     D $Ok             S              1    inz(*off)
014000081013     d $First          s               n   inz
014100040317      *
014200020212     D XX              S              3  0 inz
014300040317     D XY              S              3  0 inz
014400081013     d XW              s              3  0 inz
014500020103
014600020103      *****************************************************************
014700020103      *                                                               *
014800020103      *   C O S T A N T I                                             *
014900020103      *                                                               *
015000020103      *****************************************************************
015100020103
015200020103     D DIGITn          C                   CONST('0123456789')
015300081013
015400081013       //--------------------------------------------------------------
015500081013       //?Definizione procedure usate.
015600081013       //--------------------------------------------------------------
015700081013
015800081013       // - Controllo/Inversione date
015900081013     d xsrda8          pr                  extpgm('XSRDA8')
016000081013     d  WLBdat                             likeds(WLBdat)
016100001122
016200011211      *****************************************************************
016300011211      *                                                               *
016400011211      *   D E F I N I Z I O N E   K E Y - L I S T                     *
016500011211      *                                                               *
016600011211      *****************************************************************
016700011211
016800011211      * file FLTR704L
016900011211     C     keyTR7        klist
017000020624     C                   kfld                    TR7fil
017100020103     C                   kfld                    TR7ctl
017200020103     C                   kfld                    TR7tim
017300020624     C     k02TR704      klist
017400020624     C                   kfld                    TR7fil
017500020624     C                   kfld                    TR7ctl
017600011211      * file FNBLT27L
017700011211     C     keyBLT        klist
017800011211     C                   kfld                    BLTfls
017900011211     C                   kfld                    BLTlna
018000011211     C                   kfld                    BLTnrs
018100011211     C                   kfld                    BLTnsc
018200170131      * file FiarS03L
018300051014     C     keyarS        klist
018500051014     C                   kfld                    arStrc
018501170131     C                   kfld                    arSnot
018600011211      * file FNBLP01L
018700011211     C     keyBLP        klist
018800011211     C                   kfld                    BLPaas
018900011211     C                   kfld                    BLPlnp
019000011211     C                   kfld                    BLPnrs
019100011211     C                   kfld                    BLPnsp
019200011211
019300001122      *****************************************************************
019400001122      *                                                               *
019500011211      *   R O U T I N E   P R I N C I P A L E                         *
019600001122      *                                                               *
019700001122      *****************************************************************
019800020205      *                                                               *
019900020205      *   Riepilogo indicatori utilizzati:                            *
020000020205      *                                                               *
020100020624      *   07 - Non stampo "P.O. IN GESTIONE" (solo sede).             *
020200020205      *   10 - Lavoro                                                 *
020300020205      *   99 - Record da NON elaborare                                *
020400020205      *                                                               *
020500020205      *****************************************************************
020600001122
020700011211     C     *ENTRY        plist
020800011211     C                   parm                    KPJBA
020900011211      *
021000011211     C                   movel     KPJBU         FNLS13DS
021100020102      *
021200040317      * Reperimento dati job
021300040317     c                   exsr      DatiJob
021400020624      *
021500020624      * Non stampo "P.O. IN GESTIONE" se sono in sede
021600040317     c     DUTlpo        comp      'S'                                    07
021700011211      *
021800011211      * Preparazione dati di testata
021900081014     c                   eval      T1pgm = SDSpgm
022000011211     C                   reset                   WLBDAT
022100011211     C                   z-add     D13dda        G02INV
022200011211     C                   call      'XSRDA8'
022300011211     C                   parm                    WLBDAT
022400011211     C                   z-add     G02dat        T1dal
022500011211     C                   reset                   WLBDAT
022600011211     C                   z-add     D13dal        G02INV
022700011211     C                   call      'XSRDA8'
022800011211     C                   parm                    WLBDAT
022900011211     C                   z-add     G02dat        T1al
023000020624     C                   move      D13pog        T1pog
023100020102     C                   write     LS13T1
023200020102     C                   movel     *off          *in31
023300020103     C                   clear                   F1TINV
023400011211      *
023500011211      * Posizionamento iniziale sul file FLTR704L
023600020624     C                   move      D13pog        TR7FIL
023700020102     C                   movel     '5'           TR7CTL
023800011211     C                   movel     *zeros        TR7TIM
023900011211     C                   movel     D13dda        TR7TIM
024000081013     C     keyTR7        setll     FLTR7000
024100011211      *
024200011211      * Ciclo di lettura del file FLTR704L
024300011211      *
024400011211do  1C                   do        *hival
024500011211      *
024600081013     C     K02TR704      reade     FLTR7000
024700011211if  2C                   if        %eof(FLTR704L)
024800011211     C                             or TR7amg > D13dal
024900011211     C                   leave
025000011211e   2C                   endif
025100031118      * Includo solo letture VDL
025200081013if  2C                   if        TR7020 <> '20'
025300081013     C                   iter
025400081013e   2C                   endif
025500011211      *
025600081013      * - Reperimento altri dati  &  Selezione
025700011211     C                   exsr      srDATI
025800081013if  2C                   if        $ok <> *on
025900081013     C                   iter
026000081013e   2C                   endif
026100011211      *
026200011211      * - Stampa
026300040317     C                   clear                   xy
026400051014if  3C                   dou       xy >= %elem($FiarS)
026500040317     C                   add       1             xy
026600051014if  4C                   if        $FiarS(xy) = *blanks
026700040317     C                   leave
026800040317e   4C                   endif
026900051014     C                   movel     $FiarS(xy)    FiarSds
027000040317     C                   exsr      srDATI2
027100040317if  4C                   if        $ok = *on
027200011211     C                   exsr      STAMPA
027300040317e   4C                   endif
027400040317e   3C                   enddo
027500011211      *
027600011211e   1C                   enddo
027700011211      *
027800011211      * Fine
027900020103if  1C                   if        F1TINV = 0
028000020103     C                   write     LS13F0
028100020103x   1C                   else
028200020103     C                   write     LS13F1
028300020103e   1C                   endif
028400020103     C                   write     LS13F9
028500020103      *
028600081013     C                   movel     *on           *inLR
028700011211
028800011211      *****************************************************************
028900011211      *                                                               *
029000011211      *   S U B R O U T I N E S                                       *
029100011211      *                                                               *
029200011211      *****************************************************************
029300040317
029400040317      *---------------------------------------------------------------*
029500040317      * Reperimento Dati del job (Utente/Operativi)                   *
029600040317      *---------------------------------------------------------------*
029700040317     c     DatiJob       BEGSR
029800040317      *
029900040317     c     *dtaara       define    §azute        AZUTEds
030000040317     c     *dtaara       define    §datiute      dDatiUte
030100040317      *
030200040317     c                   in(E)     *dtaara
030300040317     c                   IF        %ERROR or RSUT = *blanks
030400040317     c                   clear                   Tibs34Ds
030500040317     c                   call      'TIBS34R'
030600040317     c                   parm                    Tibs34Ds
030700040317     c                   in        *dtaara
030800040317     c                   ENDIF
030900040317      *
031000040317     c                   ENDSR
031100011211
031200011211      *---------------------------------------------------------------*
031300011211      * srDATI * Reperimento altri dati                               *
031400011211      *---------------------------------------------------------------*
031500011211     C     srDATI        BEGSR
031600020102      *
031700020103     C                   reset                   $Ok
031800020102     C                   clear                   dsFNBLT
031900020102     C                   clear                   dsFNBLT1
032000040317     C                   clear                   dsFNBLT2
032100051014     C                   clear                   $FiarS
032200040317     C                   clear                   xy
032300020103      *
032400020103      * selezioni iniziali
032500020103if  1C                   if            TR7pd1 = *blanks
032600020103     C                             or  TR7pd2 = *blanks
032700020103     C                   goto      End_Dati
032800020103e   1C                   endif
032900020103      *
033000020103      * ================= PD1 ================= *
033100020103      *
033200020103      * Estrapolazione barcode Bartolini (Scanner)
033300020102     C                   movel     TR7pd1        a29
033400020103     C                   move      a29           a08dx
033500011211     C     '???'         scan      a29:1                                  99
033600020103if  1C                   if            a08dx  =  *zeros
033700020103     C                             or  *in99  =  *on
033800020102     C                   goto      End_Dati
033900011211e   1C                   endif
034000020205      *
034100020205     C                   move      TR7pd1        wPD1
034200020205     C                   exsr      ChkBar
034300020205if  1C                   if        *in99  =  *on
034400020205     C                   goto      End_Dati
034500020205e   1C                   endif
034600020205      *
034700020115      * - sostituzione caratteri non validi
034800020103     C     '#':' '       xlate(p)  TR7pd1        wPDx
034900020115if  2C                   if        wPDx <> *blanks
035000020115      * - reperimento posizione iniziale valida
035100020212     C     ' '           check     wPDx          XX
035200020115      * - estrapolazione dati
035300020212     C                   eval      wSCBartol  = %subst(wPDx:XX)
035400020115x   2C                   else
035500020115     C                   clear                   wSCBartol
035600020115e   2C                   endif
035700011211      *
035800011211      * Aggancio file FNBLT27L per reperire la chiave della spedizione
035900020103     C                   move      TR7pd1        wPD1
036000011211     C                   move      LNP           BLTfls
036100011211     C                   move      LNA           BLTlna
036200011211     C                   move      NRS           BLTnrs
036300011211     C                   move      NSC           BLTnsc
036400081013     C     keyBLT        chain     FNBLT000
036500011211if  1C                   if        %found(FNBLT27L)
036600020103     C                   movel     dsFNBLT       dsFNBLT1
036700011211e   1C                   endif
036800011211      *
036900020102      * ================= PD2 ================= *
037000020102      *
037100020103      * Estrapolazione "chi sono" (LaserGun)
037200020103     C                   clear                   wChiSono
037300020103      * - sostituzione caratteri non validi
037400020103     C     '#':' '       xlate(p)  TR7pd2        wPDx
037500020115if  1C                   if        wPDx <> *blanks
037600020103      * - reperimento posizione iniziale valida
037700020212     C     ' '           check     wPDx          XX
037800020103      * - estrapolazione dati
037900020212     C                   eval      wChiSono = %subst(wPDx:XX)
038000020115e   1C                   endif
038100020103      *
038200020103      * Confronto con segnacollo Bartolini
038300020103if  1C                   if        wChiSono = wSCBartol
038400020103     C                   goto      End_Dati
038500020103e   1C                   endif
038600011211      *
038700170131      * Reperisco dettaglio segnacolli da file FiarS03L
038800051014     C                   eval      arSnot = wChiSono
038900051014     C                   eval      arStrc = 'C'
039000081013     C     keyarS        setll     FiarS000
039100081013     C     keyarS        reade     FiarS000
039200170131do  1C                   dow       NOT %eof(FiarS03L)
039300040317     C                   exsr      srDATI2
039400051014     c*
039500051014     c* Se non trovata la bolla (probabilmente si tratta di ARS legato a
039600051014     c*  FNART) vado avanti a leggere
039700051014     c* (mentre prima se non trovata fnblt, dando per scontato di trovarlo
039800051014     c*  sempre, usciva dalla lettura)
039900051014     c                   if        Esistebolla='S'
040000040317if  2C                   if        $Ok = *off
040100040317     C                   leave
040200040317e   2C                   endif
040300040317     C                   add       1             xy
040400051014     C                   movel     FiarSds       $FiarS(xy)
040500051014     c                   endif
040600051014     c
040700081013     C     keyarS        reade     FiarS000
040800040317e   1C                   enddo
040900040317      *
041000040317     C     End_Dati      ENDSR
041100040317
041200040317      *---------------------------------------------------------------*
041300040317      * srDATI2 * Reperimento altri dati (2ª parte)                   *
041400040317      *---------------------------------------------------------------*
041500040317     C     srDATI2       BEGSR
041600040317      *
041700040317     C                   reset                   $Ok
041800040317     C                   clear                   dsFNBLT2
041900051014     c                   clear                   Esistebolla       1
042000020103      *
042100020103      * Aggancio file FNBLT27L per reperire la chiave della spedizione
042200051014     C                   eval      BLTfls = arSfls
042300051014     C                   eval      BLTlna = arSlna
042400051014     C                   eval      BLTnrs = arSnrs
042500051014     C                   eval      BLTnsc = arSnsc
042600081013     C     keyBLT        chain     FNBLT000
042700051014if  1C                   if        %found(FnBLT27L)
042800020103     C                   movel     dsFNBLT       dsFNBLT2
042900051014     c                   eval      Esistebolla='S'
043000020218x   1C                   else
043100051014     c                   eval      Esistebolla='N'
043200051018     C                   eval      $Ok = *on
043300040317     C                   goto      End_Dati2
043400020218e   1C                   endif
043500020103      *
043600020103      * confronto campi derivanti da PD1 con quelli derivati da PD2
043700020103if  1C                   if             pBLTfls =  aBLTfls
043800020103     C                             and  pBLTlna =  aBLTlna
043900020103     C                             and  pBLTnrs =  aBLTnrs
044000020103     C                             and  pBLTnsc =  aBLTnsc
044100040317     C                   goto      End_Dati2
044200020103e   1C                   endif
044300040317      *
044400040317     C                   eval      $Ok = *on
044500040317      *
044600040317     C     End_Dati2     ENDSR
044700020205
044800020205      *---------------------------------------------------------------*
044900020205      * CHKBAR * Controllo correttezza barcode                        *
045000020205      *---------------------------------------------------------------*
045100020205     C     CHKBAR        BEGSR
045200020205      *
045300020205      * controlla la linea di partenza segnacollo
045400020212     C                   eval      *in10 = *off
045500020212     C     DigitN        check     LNP                                    10
045600020205sel 1C                   select
045700020212w   1C                   when      *in10
045800020212     C                   eval      *in99 = *on
045900020205w   1C                   when      LNP   > *zeros
046000020205     C                   move      LNP           ORGfil
046100020212     C                   exsr      CtrORG
046200020205x   1C                   other
046300020205     C                   eval      *in99 = *on
046400020205e   1C                   endsl
046500020205      *
046600020205      * controlla la linea di arrivo segnacollo
046700020212     C                   eval      *in10 = *off
046800020212     C     DigitN        check     LNA                                    10
046900020205sel 1C                   select
047000020212w   1C                   when      *in10
047100020212     C                   eval      *in99 = *on
047200020205w   1C                   when      LNA   > *zeros
047300020212if  2C                   if        LNA  <> LNP
047400020205     C                   move      LNA           ORGfil
047500020212     C                   exsr      CtrORG
047600020212e   2C                   endif
047700020205x   1C                   other
047800020205     C                   eval      *in99 = *on
047900020205e   1C                   endsl
048000020205      *
048100020205      * controlla la serie segnacollo
048200020212     C                   eval      *in10 = *off
048300020212     C     DigitN        check     NRS                                    10
048400020205sel 1C                   select
048500020212w   1C                   when      *in10
048600020212     C                   eval      *in99 = *on
048700040317w   1C                   when      NRS  >= *zeros
048800020205x   1C                   other
048900020205     C                   eval      *in99 = *on
049000020205e   1C                   endsl
049100020205      *
049200020205      * controlla il numero segnacollo
049300020212     C                   eval      *in10 = *off
049400020212     C     DigitN        check     NSC                                    10
049500020205sel 1C                   select
049600020212w   1C                   when      *in10
049700020212     C                   eval      *in99 = *on
049800020205w   1C                   when      NSC   > *zeros
049900020205x   1C                   other
050000020205     C                   eval      *in99 = *on
050100020205e   1C                   endsl
050200020212      *
050300020212      * controlla la zona consegna segnacollo
050400020212     C                   eval      *in10 = *off
050500020212     C     DigitN        check     ZNC                                    10
050600020205sel 1C                   select
050700020212w   1C                   when      *in10
050800020212     C                   eval      *in99 = *on
050900040317w   1C                   when      ZNC  >= *zeros
051000020205x   1C                   other
051100020205     C                   eval      *in99 = *on
051200020205e   1C                   endsl
051300020205      *
051400020205     C                   ENDSR
051500020212
051600020212      *---------------------------------------------------------------*
051700020212      * CTRORG * Controllo P.O. (LNP/LNA/FLS) sul file AZORG01L       *
051800020212      *---------------------------------------------------------------*
051900020212     C     CTRORG        BEGSR
052000020212      *
052100020212      * Controllo se già memorizzato in schiera
052200020212     C                   eval      *in10 = *off
052300020212     C                   eval      XX    = 1
052400020212     C     ORGfil        lookup    $PO(xx)                                10
052500020212      *
052600020212if  1C                   if        *in10
052700020212      * - già memorizzato
052800020212     C                   movel     $POD(xx)      AZORGDS
052900020212      *
053000020212x   1C                   else
053100020212      * - non ancora memorizzato
053200020212     C     *zeros        lookup    $PO(xx)                                10
053300020212if  2C                   if        *in10
053400020212     C                   eval      $PO(xx) = ORGfil
053500020212e   2C                   endif
053600020212     C     ORGfil        chain     AZORG01L
053700020212if  2C                   if        not %found(AZORG01L)
053800020212     C                   clear                   AZORGDS
053900020212e   2C                   endif
054000020212if  2C                   if        *in10
054100020212     C                   movel     AZORGDS       $POD(xx)
054200020212e   2C                   endif
054300020212      *
054400020212e   1C                   endif
054500020212      *
054600020212      * Controllo dati del record reperito
054700020212if  1C                   if            ORGfva <> *blanks
054800020212     C                             or (ORGfag <> 'A'
054900020212     C                             and ORGfag <> 'F')
055000020212     C                   eval      *in99 = *on
055100020212e   1C                   endif
055200020212      *
055300020212     C     EndCtrOrg     ENDSR
055400020102
055500020102      *---------------------------------------------------------------*
055600020102      * STAMPA * Stampa                                               *
055700020102      *---------------------------------------------------------------*
055800020102     C     STAMPA        BEGSR
055900020102      *
056000020102     C                   if        *in31
056100020104     C                   write     LS13D9
056200020102     C                   write     LS13T1
056300020218     C                   eval      *in31  = *off
056400020102     C                   endif
056500020102      *
056600020102     C                   clear                   LS13D1
056700040317if  1C                   if        xy     = 1
056800020103     C                   eval      D1PD1  = wSCBartol
056900081014     c                   if        pBLTaas > *zero
057000081014     C                   move      pBLTaas       D1AAS1
057100081014     c                   endif
057200020102     C                   eval      D1LNP1 = pBLTlnp
057300020102     C                   eval      D1NRS1 = pBLTnrs
057400020102     C                   eval      D1NSP1 = pBLTnsp
057500020103     C                   eval      D1LNA1 = pBLTlna
057600081013      * Decodifica mittente da BarCode Bartolini
057700040317     C                   eval      BLPaas = pBLTaas
057800040317     C                   eval      BLPlnp = pBLTlnp
057900040317     C                   eval      BLPnrs = pBLTnrs
058000040317     C                   eval      BLPnsp = pBLTnsp
058100081013     C     keyBLP        chain     FNBLP000
058200040317if  2C                   if        %found(FNBLP01L)
058300040317     C                   reset                   WLBDAT
058400040317     C                   movel     *year         G02INV
058500040317     C                   move      BLPmgs        G02INV
058600040317     C                   call      'XSRDA8'
058700040317     C                   parm                    WLBDAT
058800040317     C                   movel     G02dat        D1GMS1
058900040317     C                   eval      D1RSM1 = BLPrsm
059000040317     C                   eval      D1NCL1 = BLPncl
059100040317e   2C                   endif
059200040317e   1C                   endif
059300040317      *
059400020103     C                   eval      D1PD2  = wChiSono
059500081013     c                   eval      D1HHSP = TR7hms
059600081014     c                   if        aBLTaas > *zero
059700081014     C                   move      aBLTaas       D1AAS2
059800081014     c                   endif
059900020103     C                   eval      D1LNP2 = aBLTlnp
060000020103     C                   eval      D1NRS2 = aBLTnrs
060100020103     C                   eval      D1NSP2 = aBLTnsp
060200020103     C                   eval      D1LNA2 = aBLTlna
060300081013      * Decodifica mittente da "Chi Sono"
060400020103     C                   eval      BLPaas = aBLTaas
060500020103     C                   eval      BLPlnp = aBLTlnp
060600020103     C                   eval      BLPnrs = aBLTnrs
060700020103     C                   eval      BLPnsp = aBLTnsp
060800081013     C     keyBLP        chain     FNBLP000
060900020103if  1C                   if        %found(FNBLP01L)
061000020221     C                   reset                   WLBDAT
061100020221     C                   movel     *year         G02INV
061200020221     C                   move      BLPmgs        G02INV
061300020221     C                   call      'XSRDA8'
061400020221     C                   parm                    WLBDAT
061500020221     C                   movel     G02dat        D1GMS2
061600020103     C                   eval      D1RSM2 = BLPrsm
061700020218     C                   eval      D1NCL2 = BLPncl
061800020103e   1C                   endif
061900020218      *
062000020218      * "Costruzione" Segnacollo
062100020218     C                   clear                   wPD3
062200020221     C                   move      aBLTlnp       LNP3
062300020221     C                   move      aBLTlna       LNA3
062400020221     C                   move      aBLTnrs       NRS3
062500020221     C                   move      aBLTnsc       NSC3
062600020218     C                   move      BLPznc        ZNC3
062700020218     C                   eval      D1SGNC = BAR3
062800020102      *
062900081013     C                   write     LS13D0
063000020102     C                   write     LS13D1
063100020103     C                   add       1             F1TINV
063200081013      /free
063300081013       // Ricerca/Stampa di altre letture VDL legate allo stesso "Chi Sono"
063400081013         if  %subst(KNMUS : 1 : 3) = 'EDP';
063500081013           exsr  sr_SQLxPD2;
063600081013         endif;
063700081013      /end-free
063800020102      *
063900020102     C                   ENDSR
064000081013
064100081013      /free
064200081013       //--------------------------------------------------------------
064300081013       //?Ricerca di altre letture VDL legate allo stesso "Chi Sono"
064400081013       //--------------------------------------------------------------
064500081013       BEGSR  sr_SQLxPD2;
064600081013
064700081013         exec sql   set option  dynusrprf = *owner,
064800081013                                closqlcsr = *endmod;
064900081013
065000081013         $First   = *on;
065100081013
065200081013         StringaSQL = 'select * +
065300081014                       from   FLTR703L +
065400081013                       where  TR7fil = ''' + TR7fil + '''' +
065500081013                       ' and  TR7ctl = ''' + TR7ctl + '''' +
065600081013                       ' and  TR7nfv = ''' + TR7nfv + '''' +
065700081013                       ' and  TR7pd2 like ''%' + %trim(wChiSono) + '%''' +
065800081013                       ' and (TR7ctl concat TR7npl concat TR7nfv concat +
065900081013                              TR7tim concat TR7mil concat TR7cod concat +
066000081013                              TR7020 concat TR7pic) <> ''' +
066100081013                       TR7ctl + TR7npl + TR7nfv + TR7tim + TR7mil +
066200081013                       TR7cod + TR7020 + TR7pic + '''';
066300081013
066400081013         exec sql   prepare  S1  from :StringaSQL;
066500081013
066600081013         exec sql   declare  C1  cursor for  S1;
066700081013
066800081013         exec sql   open  C1;
066900081013
067000081013         DOU  SQLcod < *zero   or   SQlcod = 100;
067100081013
067200081013           exec sql   fetch  C1  into :ds_FLTR7;
067300081013
067400081013           select;
067500081013             when  SQLcod = 100;
067600081013             when  SQlcod < *zero;
067700081013             other;
067800081013               exsr  Stampa2;
067900081013           endsl;
068000081013
068100081013         ENDDO;
068200081013
068300081013         exec sql   close  C1;
068400081013
068500081013       ENDSR;
068600081013
068700081013       //--------------------------------------------------------------
068800081013       //?Stampa singola letture VDL legate allo stesso "Chi Sono"
068900081013       //--------------------------------------------------------------
069000081013       BEGSR  Stampa2;
069100081013
069200081014         // Reperimento altri dati
069300081014         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
069400081014         exsr  srDati3;
069500081013
069600081013         // Ciclo di stampa
069700081013         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
069800081014         FOR  xw = 1  to %elem($Fiars2);
069900081013
070000081014           if  $Fiars2(xw) = *blank;
070100081013             leave;
070200081013           endif;
070300081013
070400081014           FIARSds = $Fiars2(xw);
070500081014           exsr  srDati4;
070600081013
070700081013           // Costruzione segnacollo per selezione dei soli records
070800081013           // ¯ con segnacollo OK ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
070900081013           //   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
071000081013           if  $Ok = *on;
071100081013             chain  (aBLTaas : aBLTlnp : aBLTnrs : aBLTnsp)  FNBLP000;
071200081013             if  NOT  %found(FNBLP01L);
071300081013               clear  BLPznc;
071400081013             endif;
071500081013             clear  wPD3;
071600081013             LNP3 = %editc(aBLTlnp : 'X');
071700081013             LNA3 = %editc(aBLTlna : 'X');
071800081013             NRS3 = %editc(aBLTnrs : 'X');
071900081013             NSC3 = %editc(aBLTnsc : 'X');
072000081013             ZNC3 = %editc(BLPznc : 'X');
072100081013             // Selezione dei soli segnacolli = barcode Bartolini
072200081013             // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
072300081013             if  Bar <> Bar3;
072400081013               $Ok = *off;
072500081013             endif;
072600081013           endif;
072700081013
072800081013           // Scarta i records da non stampare
072900081013           // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
073000081013           if  $Ok = *off;
073100081013             iter;
073200081013           endif;
073300081013
073400081013           // Imposta i campi da stampare
073500081013           // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
073600081013           clear  LS13D1;
073700081013
073800081013           // Impostazione dati per "Chi Sono"
073900081014           // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
074000081013           D1PD2  = wChiSono;
074100081014           TR7tim = xTR7tim;
074200081014           D1HHSP = TR7hms;
074300081014           if  aBLTaas > *zero;
074400081014             D1AAS2 = %subst( %char(aBLTaas) : 3);
074500081014           endif;
074600081013           D1LNP2 = aBLTlnp;
074700081013           D1NRS2 = aBLTnrs;
074800081013           D1NSP2 = aBLTnsp;
074900081013           D1LNA2 = aBLTlna;
075000081013           // Decodifica mittente da "Chi Sono"
075100081013           // (chain già eseguita)
075200081013           //chain  (aBLTaas : aBLTlnp : aBLTnrs : aBLTnsp)  FNBLP000;
075300081013           If  %found(FNBLP01L);
075400081013             reset  WLBdat;
075500081013             G02inv = (*year * 10000) + BLPmgs;
075600081013             XSRDA8 (WLBdat);
075700081014             D1GMS2 = (G02dat - *year) / 10000;
075800081014             D1RSM2 = BLPrsm;
075900081014             D1NCL2 = BLPncl;
076000081013           EndIf;
076100081013
076200081013           // Impostazione Segnacollo
076300081013           D1sgnc = BAR3;
076400081013
076500081013           // Impostazione dati per barcode Bartolini
076600081014           // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
076700081013           If  $First = *on;
076800081013             D1PD1  = wSCBartol;
076900081014             if  pBLTaas > *zero;
077000081014               D1AAS1 = %subst( %char(pBLTaas) : 3);
077100081014             endif;
077200081013             D1LNP1 = pBLTlnp;
077300081013             D1NRS1 = pBLTnrs;
077400081013             D1NSP1 = pBLTnsp;
077500081013             D1LNA1 = pBLTlna;
077600081013             // Decodifica mittente da BarCode Bartolini
077700081013             chain  (pBLTaas : pBLTlnp : pBLTnrs : pBLTnsp)  FNBLP000;
077800081013             if  %found(FNBLP01L);
077900081013               reset  WLBdat;
078000081013               G02inv = (*year * 10000) + BLPmgs;
078100081013               XSRDA8 (WLBdat);
078200081014               D1GMS1 = (G02dat - *year) / 10000;
078300081014               D1RSM1 = BLPrsm;
078400081014               D1NCL1 = BLPncl;
078500081013             endif;
078600081013           EndIf;
078700081013
078800081013           // Stampa singolo record
078900081013           if  *in31;
079000081013             write  LS13D9;
079100081013             write  LS13T1;
079200081013             *in31 = *off;
079300081013           endif;
079400081013
079500081013           write  LS13D1;
079600081013
079700081013           $First = *off;
079800081013
079900081013         ENDFOR;
080000081013
080100081013       ENDSR;
080200081014
080300081014       //--------------------------------------------------------------
080400081015       //?Selezione letture segnacollo
080500081014       //--------------------------------------------------------------
080600081014       BEGSR  srDati3;
080700081014
080800081014         clear  wSCBartol;
080900081014         clear  wChiSono;
081000081014         clear  $FIARS2;
081100081015         clear  xw;
081200081015         *in99 = *off;
081300081014
081400081014         // Elaborazione PD1 - BarCode Bartolini (Scanner)
081500081014         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
081600081014         // - Estrapolazione barcode Bartolini (Scanner)   &
081700081014         //   Controllo validità
081800081014         a29   = %subst(xTR7pd1 : 1 : %len(a29));
081900081014         if  %subst(a29 : %len(a29) - 7 : 8) = *zero   or
082000081014             %scan('???' : %trimr(a29))      > *zero;
082100081014           leavesr;
082200081014         endif;
082300081014         wPD1 = %subst(xTR7pd1 : %len(xTR7pd1) - %len(wPD1) + 1);
082400081014         exsr  ChkBar;
082500081014         if  *in99  =  *on;
082600081014           leavesr;
082700081014         endif;
082800081014
082900081014         // - Sostituzione caratteri non validi
083000081014         wPDx = %xlate ('#' : ' ' : xTR7pd1);
083100081014         // - Estrapolazione dati dalla posizione iniziale valida
083200081014         if  wPDx <> *blank;
083300081014           wSCBartol  = %subst (wPDx : %check(' ' : wPDx));
083400081014         endif;
083500081014
083600081014         // - Aggancio file FNBLT27L per reperire la chiave della spedizione
083700081014         // wPD1 = %subst(xTR7pd1 : %len(xTR7pd1) - %len(wPD1) + 1); (già fatto)
083800081014         BLTfls = %int(LNP);
083900081014         BLTlna = %int(LNA);
084000081014         BLTnrs = %int(NRS);
084100081014         BLTnsc = %int(NSC);
084200081014         chain  keyBLT  FNBLT000;
084300081014         if  %found(FNBLT27L);
084400081014           dsFNBLT1 = dsFNBLT;
084500081014         endif;
084600081014
084700081014         // Elaborazione PD2 - "Chi Sono" (LaserGun)
084800081014         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
084900081014         // - Sostituzione caratteri non validi
085000081014         wPDx = %xlate ('#' : ' ' : xTR7pd2);
085100081014         // - Estrapolazione dati dalla posizione iniziale valida
085200081014         if  wPDx <> *blank;
085300081014           wChiSono  = %subst (wPDx : %check(' ' : wPDx));
085400081014         endif;
085500081014
085600170131         // - Reperimento dettaglio segnacolli da file FIARS03L
085700081014         ARSnot = wChiSono;
085800081014         ARStrc = 'C';
085900081014         setll  keyARS  FIARS000;
086000081014         reade  keyARS  FIARS000;
086100081014
086200170131         DOW  NOT  %eof(FiarS03L);
086300081014
086400081014           exsr  srDati4;
086500081014
086600081014           // - Se non trovata la bolla (probabilmente si tratta di ARS
086700081014           //   legato a FNART) va avanti a leggere
086800081014           if  EsisteBolla = 'S';
086900081014             if  $Ok = *off;
087000081014               leave;
087100081014             endif;
087200081014             xw += 1;
087300081014             $FIARS2(xw) = FIARSds;
087400081014           endif;
087500081014
087600081014           reade  keyARS  FIARS000;
087700081014
087800081014         ENDDO;
087900081014
088000081014       ENDSR;
088100081014
088200081014       //--------------------------------------------------------------
088300081014       //?Reperimento altri dati (4ª parte)
088400081014       //--------------------------------------------------------------
088500081014       BEGSR  srDati4;
088600081014
088700081014         reset  $Ok;
088800081014         clear  dsFNBLT2;
088900081014         clear  EsisteBolla;
089000081014
089100081014         // Aggancio file FNBLT27L per reperire la chiave della spedizione
089200081014         BLTfls = ARSfls;
089300081014         BLTlna = ARSlna;
089400081014         BLTnrs = ARSnrs;
089500081014         BLTnsc = ARSnsc;
089600081014         chain  keyBLT  FNBLT000;
089700081015         if  %found(FNBLT27L);
089800081014           dsFNBLT2 = dsFNBLT;
089900081014           EsisteBolla = 'S';
090000081014           $Ok = *on;
090100081015         else;
090200081014           EsisteBolla = 'N';
090300081014           leavesr;
090400081015         endif;
090500081014
090600081014         // Confronto campi derivanti da PD1 con quelli derivati da PD2
090700081015         if  pBLTfls <> aBLTfls   or
090800081014             pBLTlna <> aBLTlna   or
090900081014             pBLTnrs <> aBLTnrs   or
091000081014             pBLTnsc <> aBLTnsc;
091100081014           leavesr;
091200081015         endif;
091300081014
091400081014         $Ok = *on;
091500081014
091600081014       ENDSR;
091700081013      /end-free
