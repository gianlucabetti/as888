000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400060609      **?************************************************************************
000500160316      *  Da SPUNTA VDL    fibarDPD0F                                            *
000600160316      *  BOLLE IMPORT DPD  genericamente scrive il VAB da confermare in ARRIVO  *
000700160317      * ?Service  Code:  101-Normal Parcel
000800160317      * ?                136-Small Parcel
000900160317      * ?                298-Return Sender
001000160317      * ?                300-Return
001100160317      * ?                327-Normal Parcel B2C
001200160317      * ?                329-Normal Parcel B2C
001300160317      * ?                328-Small  Parcel B2C
001400160317      * ?                365-Tyres
001500160317      * ?                366-Tyres B2C
001600160317      * ?                367-Tyres COD
001700060612      **?************************************************************************
001800991206     FFLNUF01L  UF   E           K DISK
001900130530     FFnorm01L  iF   E           K DISK
002000991129     FTNTBE01L  IF   E           K DISK
002100050112     FTABEL00F  IF   E           K DISK
002200160920     Ffibsp01l  iF   E           K DISK
002300130618     FazORG01L  IF   E           K DISK
002400971216      *
002500150604     FTNCLD00F  IF   E           K DISK
002600061016     Ffidpo06l  IF   E           K DISK
002700160318     Fdpcdp01l  IF   E           K DISK
002800160318     Fdppdc01l  IF   E           K DISK
002900061016      *
003000050414     FFiVAB01L  UF A E           K DISK
003100050414     FEDivab3L  UF A E           K DISK
003200041215      *
003300050414     FEDiVAT3L  Uf A E           K DISK
003400050414     FFiVAT01L  Uf A E           K DISK
003500160317      *----------------------------------------------------*
003600160317     D TRUL06DS      E DS
003700160317     D  LIN                    1     90  0 DIM(30)
003800940321      *----------------------------------------------------*
003900991124      * Schiere per conversione peso DPD
004000991124     D DPPK            S             15    DIM(30)                              CODICE NAZIONE
004100991124     D DPP             S              7S 1 DIM(30)                              CODICE NAZIONE
004200141210     D VLB             S              5S 3 DIM(30)                              CODICE NAZIONE
004300160629      *-------------------
004400160629     D DBLP01        E DS
004500160629     D DBLP          E DS
004600160629     D DTASV         E DS
004700160629     D trul90ds      E DS
004800160916     D DVPOFIEU21    E DS
004900160916     D  VPO21R1                1     60  0 DIM(20)
005000160317      *-------------------
005100160317     D KPJBA         E DS
005200160317     D OG143         E DS
005300160317     D DDPP          E DS
005400160317     D DLPD          E DS
005500160318     D DNSD          E DS
005600160317     D tisie2ds      E DS
005700160317     D DpdcFLO       E DS
005800160317     D FIEULPDDS     E DS
005900160718      *--------------------------
006000160718     d* Parametri x Controllo profilo utenti
006100160718     d TIBS34ds      e ds
006200160718     d AZUTEds       e ds                  extname(AZUTE00F)
006300160718     d DDatiUte      e ds
006400160718     D*------------------
006500160718     D* DS REPEIMENTO TERMINALs
006600160718     D*------------------
006700160718     D FNLV55DS      E DS
006800160718      *---------------------
006900160718      *--------------------------
007000991129     D W015A           S             15
007100091109     D W030A           S             30
007200000223     D W0140           S             14  0
007300991129     D WORA            S              6  0
007400991129     D XX              S              3  0 INZ
007500991129     D WDTGIO          S              8  0
007600991129     D DATEU           S              8  0
007700991129     D DATA_eur        S               D   DATFMT(*eur)
007800050112     D svkpjbu         s                   like(kpjbu)
007900060608      **?------------------------------------------------------------------ */
008000060612     D DVate         E DS
008100060620      *
008200160316     D DsBareCode      DS
008300160316     d  Percent                1      1
008400160316     d  CAPdest                2      8
008500160316     D  DepotParcel            9     12
008600160316     d  NrParcel               9     22
008700160316     d  ServCode              23     25
008800160316     d  Nazione               26     28
008900060608      **?------------------------------------------------------------------ */
009000050112      * Numeratore Bolle (302)
009100050112     D trul33ds      E DS
009200050112     D Ds3C          E DS
009300070202     D TISI95DS      E DS
009400160317     D KAAA            S                   LIKE(NUFAAA)
009500160317     D KCNU            S                   LIKE(NUFCNU)
009600160317     D KFIL            S                   LIKE(NUFFIL)
009700160317     D NUM_Sped        s                   LIKE(vabnsp)
009800060612      * ?   *--------------------------------------------------------------*
009900140827      **
010000060710     D Digits          C                   '0123456789'
010100160317     d Normal_Parcel   c                   '101'
010200160317     d Small_Parcel    c                   '136'
010300160317     d Return_Sender   c                   '298'
010400160317     d Return          c                   '300'
010500160317     d Gomme_365       c                   '365'
010600160317     d Gomme_366       c                   '366'
010700160317     d Gomme_367       c                   '367'
010800111110     d Normal_Parcel_327...
010900160317     d                 c                   '327'
011000111110     d Normal_Parcel_329...
011100160317     d                 c                   '329'
011200111110     d Small_Parcel_328...
011300160317     d                 c                   '328'
011400060612      * ?================================================================== */
011500160316     c                   SETON                                        LR
011600160316     c                   eval      DsBareCode = barecode28
011700160316      **
011800160316      **  Reso NON si scrive
011900160317     c                   if        ServCode  = Return_Sender
012000160317     c                               or
012100160317     c                             ServCode  = Return
012200160316     c                               or
012300160317     c                             DepotParcel = '0844'
012400160317     c                               or
012500160317     c                             DepotParcel = '0845'
012600160318      *
012700160318      * abilitato significa anche abilitato alla cancellazione del record
012800160318      *   poichè è riuscito a impostare il record bolla quindi può cancellare la spia
012900160318     c                   MOVE      'S'           abilitato         1
013000160317      * ?            ==============
013100160316     c                   return
013200160317      * ?            ==============
013300160316     c                   ENDIF
013400160317
013500160316      **?------------------------------------------------------------------ */
013600160316      * Clearizzo EDiVAB e FiVAB
013700160316     c                   clear                   edivab00
013800160316     c                   clear                   fivab000
013900160316     c                   clear                   su_EDIVAB         1
014000060612      **?------------------------------------------------------------------ */
014100160316      * ?Data  spedizione
014200160721     C                   MOVE      datcor        VABMGS                         = oggi
014300160317      * ?Tipo servizio/Colli
014400160316     c                   movel     'C'           vabtsp
014500160316     c                   z-add     1             vabncl
014600160721     C                   MOVEL     DATCOR        CAMPO4            4
014700160721     C                   MOVE      CAMPO4        VABAAS
014800160316     c
014900160316      * ?il Parcel deve essere impostato nella DS VATE
015000160316      * fisso il rif.numerico a (1) e sostituito se contiene solo numeri
015100160316     C                   z-add     1             vabrmn
015200160317     c     digits        check     NrParcel      position          3 0
015300160316      **  presenti campi non numerici: (Ossia trovati)
015400160606     c                   iF         %Found
015500160606     c                   MOVE      'S'           abilitato         1
015600160606     c                   RETURn
015700160606     c                   else
015800160606      * ?  per impostare il riferimento numerico (VABRMN):
015900160606     C                   move      NrParcel      vabrmn
016000160606     c                   enD
016100160317      * ?CAP
016200160318     c                   move      CAPdest       field5            5            C.A.P.
016300160318     c                   movel     field5        vabcad                         C.A.P.
016400160316      * ?Se manca la provincia la calcolo
016500160316     C                   CLEAR                   TISI95DS
016600160316     C                   MOVEL     vabCAD        I95CAP
016700160316     C                   MOVEL     DATEU         I95DAT
016800160316     C                   MOVEL     '3'           I95TCN
016900160316     C                   CALL      'TISI95R'
017000160316     C                   PARM                    TISI95DS
017100160316     C     O95ERR        IFeq      *BLANKS
017200160316     C                   MOVEL     O95PRV        vabPRD
017300160329     C                   Else
017400160329      * x errore NON scrive la spedizione
017500160329     C                   RETURN
017600160329     C                   END
017700160316      *
017800160316      ** imposta il Depot di provenienza x testare se VTG
017900160316     c                   clear                   parcel_VTG        1
018000160316     C                   movel(p)  'VTG'         cldCOD
018100160316     C                   move      DepotParcel   cldDEP
018200160316     c     key_VTG       setll     tncld00F
018300160316     c     key_VTG       reade     tncld00F
018400160316     c                   dow       not %EoF(tncld00F)
018500160316     c                   if        dateu >= cldDDA and
018600160316     c                             dateu <= cldADA
018700160316     c                   move      'S'           parcel_VTG
018800160316     c                   leave
018900160316     c                   end
019000160316     c     key_VTG       reade     tncld00F
019100160316     c                   endDO
019200160317      * ?Service  Code:
019300160316     c                   clear                   w015A
019400160317     c                   if        ServCode  = Normal_Parcel
019500160316     c                               or
019600160317     c                             ServCode  = Normal_Parcel_327
019700160317     c                               or
019800160317     c                             ServCode  = Normal_Parcel_329
019900160316     c                   eval      W015A = 'PESO01'
020000160316     c                   end
020100160317      **  traffico gomme.
020200160317     c                   if        ServCode  = Gomme_365
020300160316     c                               or
020400160317     c                             ServCode  = Gomme_366
020500160316     c                               or
020600160317     c                             ServCode  = Gomme_367
020700160316     c                   eval      W015A = 'PESO03'
020800160316     c                   end
020900160317      * piccolo
021000160317     c                   if        ServCode  = Small_Parcel
021100160316     c                               or
021200160317     c                             ServCode  = Small_Parcel_328
021300160316     c                   eval      W015A = 'PESO02'
021400160316     c                   end
021500160316      * se per qualche motivo non passano il codice pacco/peso imposto come
021600160316      *   pacco grande
021700160316     c                   if        W015A = *blank
021800160316     c                   eval      W015A = 'PESO01'
021900160316     c                   end
022000160316      *
022100160316      * ?Peso, ricavato dal tipo servizio del ROUTELABEL
022200160316     c                   z-add     1             xx
022300160316     c                   if        w015a <> *blank
022400160316     c     W015A         lookup    DPPK(xx)                               31
022500160316     c                   If        *in31 = *off
022600160316     c                   eval      W015A = 'PESO'
022700160316     c     W015A         lookup    DPPK(xx)                               31
022800160316     c                   Endif
022900160316     c                   z-add     DPP(xx)       vabPKB
023000161006     c                   z-add     vlb(xx)       vabVLB
023100160316     c                   End
023200160316      * ?Codice bolla
023300160316     C                   MOVEL     '1'           vabcbo                         NO C/assegno
023400160317      * ?Provenienza
023500160317     c                   movel     '1'           VABscl
023600160316      *
023700160316      * ?Controlla esistenza del Parcel sul file degli ORM dati a DPD per
023800160317      * ?assegnato ORM
023900160317     C     nrParcel      chain     fidpo06l
024000160316     c                   if        %Found(fidpo06l)
024100160316     C                   MOVEL     '2'           vabcbo
024200160316     c                   end
024300160316      *
024400160316      * ?SE si tratta di un ORM commissionato a DPD, Controllo se deve essere
024500160316      * ?in FERMO DEPOSITO.
024600160316     c                   eval      vabFFD = *blank
024700160316     C                   if         vabcbo = '2'  and dpoPOE > 0
024800160316     C                   z-add     dpoPOE        ORMPOE
024900160316     C                   z-add     dpoNRS        ORMNSR
025000160316     C                   z-add     dpoNOR        ORMNOR
025100160316     C                   z-add     dpoNRV        ORMNRV
025200160317      * ?sull'ORM se Fermo Deposito
025300160316     C     KORM          chain     fnORM01l
025400160316     c                   if        %Found(FnOrm01l) and ormffd ='S'
025500160316     c                   eval      vabFFD = 'S'
025600160316     c                   end
025700160316     c                   end
025800160317      **
025900160317      *--------
026000160317     C* con la Linea di chi spara prende quella DPD da sostituire su LNP e FGS
026100160317     c     Filiale       chain     azORG01l
026200160317     c                   if          %Found(azORG01l)
026300160317     c                   movel     orgde3        OG143
026400160317     c                   if           §OGLNPB <> *blank and §OGLNPB <> '000'
026500160318     c                   move      §OGLNPB       VABLNP
026600160318     c                   move      §OGLNPB       VABFGS
026700160318     c                   move      §OGLNPB       Linea_Partenza    3 0
026800160317     c                   end
026900160317     c                   end
027000160318      **
027100160318      **  ricava la LNP e altri valori fondamentali dal PARCEL/DEPOT di Partenza
027200160318     c                   exsr      LNP_da_dove
027300160318      *
027400160318      ** tab.definizione dati x Linea DPD
027500160318     c                   exsr      tabella_LPD
027600160317      *
027700160318      * da tabella NSD (1)
027800160318     c                   movel     §NSDRSD       VABRSD
027900160318     c                   movel     §NSDIND       VABIND
028000160318     c                   movel     §NSDLOD       VABLOD
028100160318      *
028200160317      * abilitato significa anche abilitato alla cancellazione del record
028300160317      *   poichè è riuscito a impostare il record bolla quindi può cancellare la spia
028400160317     c                   MOVE      'S'           abilitato         1
028500160916      *
028600160916     c                   move      'N'           scrivereVAB       1
028700160916      *
028800160916      * Controlla se VPO attivata sulla filiale o su tutte
028900160916     c     Filiale       lookup    vpo21r1                                21
029000160916     c  n21all999        lookup    vpo21r1                                21
029100160916     c  n21              eval          scrivereVAB ='S'
029200160916      *
029300160916      *  Se presente in VPO o tutto attivo "999"
029400060612      * -----------------                  -------------------
029500160720      * ?scrive SPED.VEDI PACCO in PARTENZA BLP
029600160916     C   21              EXSR      Scrive_BLP
029700160720      * -----------------                  -------------------
029800160627      *
029900151113      * ?==============================
030000060612      * ?Scrivo EDiVAB o FiVAB
030100151113      * ?==============================
030200160629     c                   if            scrivereVAB ='S'
030300160720      * -----------------                  -------------------
030400160720      * ?Numero spedizione
030500160720     C                   EXSR      SUB_keybol
030600160720      * -----------------                  -------------------
030700160317     C                   IF        su_EDIVAB = 'S'
030800160317     c                   eval      vabcmr = 'GEO_' + %editc(dateu:'Y') + '_PST'
030900060612     c                   z-add     DATEU         VABdcm
031000060612     c                   z-add     DATEU         VABdts
031100060612     c                   movel     WORA          VABhms
031200060612     c                   write     EDivab00
031300060612     c                   ELSE
031400060612     c                   write     FiVAB000
031500060612     c                   ENDIF
031600060614      ***
031700160316     c                   exsr      wrtvat
031800160720     c                   end
031900060612      *
032000160317     c                   Return
032100060612     C*----------------------------------------------------------------
032200060612     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
032300060612     C*----------------------------------------------------------------
032400060612     C     WRTVAT        BEGSR
032500060612      *
032600060413      * ? Parcel Nr. --> Riferimento Parcel DPD
032700051219     C                   If        su_EDIVAB = 'S'
032800041215     c                   clear                   edivat00
032900041215     c                   else
033000041215     c                   clear                   fivat000
033100041215     c                   end
033200160316      **  su nuovo tipo record (E)
033300160316     C                   MOVEL     'E'           VATTRC
033400041215     C                   movel     VABfgs        VATfgs
033500041215     C                   Z-ADD     VABCCM        VATCCM
033600041215     C                   Z-ADD     VABAAS        VATAAS
033700041215     C                   Z-ADD     VABLNP        VATLNP
033800041215     C                   Z-ADD     VABNRS        VATNRS
033900041215     C                   Z-ADD     VABNSP        VATNSP
034000060621      *
034100060621      *  IMPOSTA la DS VAT(E)
034200160316     c                   clear                   DVate
034300160316     C                   MOVEL     NrParcel      §VATEPRN
034400060620     C                   move      *zeros        §VATESCD
034500160316     C                   move      ServCode      §VATESCD
034600160316     C                   move(p)   '%'           §VATEBID
034700060608     C                   MOVEL     *blank        §VATEROU
034800060620      *
034900060608     C                   MOVEL     DVate         VATNOT
035000041215      **
035100051219     C                   If        su_EDIVAB = 'S'
035200041215     C                   MOVEL     VABCMR        VATCMR
035300041215     C                   Z-ADD     VABCNT        VATCNT
035400041215     C                   WRITE     EDiVAT00
035500041215     c                   else
035600041215     C                   WRITE     fiVAT000
035700041215     c                   end
035800060413      *
035900041215     C                   ENDSR
036000971215      *----------------------------------------------------------------
036100971216      * DETERMINO DATA E CHIAVE SPEDIZIONE
036200971215      *----------------------------------------------------------------
036300991206     C     SUB_keybol    BEGSR
036400050112      *
036500050112      **              **------------------**
036600050112      *    deve controllare sulla tabella "3C" se il numero spedizione
036700050112      *     deve essere mantenuto oppure no
036800050112     C                   clear                   Ds3C
036900060620     C                   Z-ADD     1             TBLKUT
037000050112     C                   MOVEL     '3C'          TBLCOD
037100050112     C                   movel(p)  VABccm        TBLKEY
037200160317     C     KTABel        CHAIN     TABEL00F
037300050112     C                   IF        %Found(TABEL00F) and tblflg = *blank
037400050112     C                   MOVEL     TBLUNI        Ds3C
037500050112     C                   END
037600050112      *
037700050112      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
037800050112     c                   if        §3CFsp <> 'S'
037900050112      *
038000050112     C* NSP => Stacco un numeratore da AZNUM
038100050112     c                   movel     kpjbu         svkpjbu
038200050112     c                   clear                   kpjbu
038300050112     C                   clear                   TRUL33DS
038400050112     C                   eval      I33OPE = *zeros
038500050112     C                   eval      I33CNU = 302
038600050112     C                   eval      I33NUM = 1
038700050112     C                   movel     TRUL33DS      KPJBU
038800050112     C                   call      'TRUL33R'
038900050112     C                   parm                    KPJBA
039000050112     C                   movel     KPJBU         TRUL33DS
039100050112     C                   if        O33ERR = *zeros
039200050112     c                   z-add     o33nrf        NUM_SPED
039300050112     C                   endif
039400050112     c                   movel     svkpjbu       kpjbu
039500050112      ****
039600050112     c                   ELSE
039700050112      *   se si vuole mantenere il numero spedizione
039800050112      ****
039900160721     C                   MOVEl     datCOR        KAAA
040000050112     C                   Z-ADD     3             KCNU
040100050112     C                   Z-ADD     VABLNP        KFIL
040200050112     C     KNUF          CHAIN     FLNUF                              9999
040300050112     C                   IF        *IN99 = *OFF
040400050112     C                   ADD       1             NUFNUM
040500050112     C                   Z-ADD     NUFNUM        NUM_Sped                       *NUM.SPEDIZIONE
040600050112     C                   UPDATE    FLNUF
040700060612     c                   END
040800060411      *
040900050112     c                   EndIF
041000050112      **
041100050112      **              **------------------**
041200050112     C                   Z-ADD     NUM_Sped      VABNSP                         *NUM.SPEDIZIONE
041300050112      **              **------------------**
041400160721     C                   MOVEL     DATCOR        CAMPO4            4
041500160721     C                   MOVE      CAMPO4        KAAA
041600160721     C                   MOVE      CAMPO4        VABAAS
041700971216      *
041800991206     C                   ENDSR
041900051205      *----------------------------------------------------*
042000051205      *   DEFINIZIONE CHIAVI                               *
042100051205      *----------------------------------------------------*
042200051205     C     *INZSR        BEGSR
042300051205      *
042400991129     c     *ENTRY        PLIST
042500160317     C                   parm                    Filiale           3 0
042600160316     C                   parm                    BareCode28       28
042700160316     C                   parm                    Abilitato         1
042800160719     C                   parm                    kpjba
042900160316      *
043000160718      *  prende i dati utente
043100160718     c                   exsr      DATIjob
043200160718      *
043300160316      *  se scriverà correttamente il VAB deve impostare "S" in abilitato
043400160316      *  che significa che il programma chiamante può cancellare la spunta
043500160316      *  poichè già utilizzata.
043600991129      *
043700991206     C     KNUF          KLIST
043800991206     C                   KFLD                    KAAA
043900991206     C                   KFLD                    KCNU
044000991206     C                   KFLD                    KFIL
044100160317      *
044200160317     C     key_Depot     KLIST
044300160317     C                   KFLD                    vers_CAP_DPD
044400160317     C                   KFLD                    DepotParcel
044500160317      *
044600160317     C     key_Dep       KLIST
044700160317     C                   KFLD                    DepotParcel
044800160317     C                   KFLD                    DATEU
044900971215      *
045000050112     C     KTABel        KLIST
045100050112     C                   KFLD                    TBLKUT
045200050112     C                   KFLD                    TBLCOD
045300050112     C                   KFLD                    TBLKEY
045400160318      *
045500160318     C     KTBE          KLIST
045600160318     C                   KFLD                    TBECOD
045700160318     C                   KFLD                    TBEKE1
045800100920      *
045900100920     C     KORM          KLIST
046000100920     C                   KFLD                    ORMPOE
046100100920     C                   KFLD                    ORMNSR
046200100920     C                   KFLD                    ORMNOR
046300100920     C                   KFLD                    ORMNRV
046400150604      *
046500150604     C     key_VTG       KLIST
046600150604     C                   KFLD                    cldCOD
046700150604     C                   KFLD                    cldDEP
046800160301      **
046900050414      *------------------
047000020919      * Apro file transito o FiVAB pilotato dalla tabella
047100991124      * Caricamento Tabella PESI DPD
047200060620     c                   clear                   XX
047300991124     c                   clear                   DPP
047400991124     c     'DPP'         chain     tntbe000                           31
047500991124      *
047600991124     c                   DOW       *in31 = *off
047700991124      * Se il S.I. è indicato deve essere uguale al mio
047800991124     c                   IF        TBEatb = *BLANKS  and
047900991129     c                              (TBEsif = Knsif  or  TBEsif = *blanks)
048000991124     c                   add       1             xx
048100991124     c                   movel     TBEke1        DPPK(XX)
048200000406     c                   movel     TBEuni        DDPP
048300000410     c                   movel     §DPPpkb       DPP(XX)
048400141210     c                   movel     §DPPvlb       vlb(XX)
048500991124     c                   endif
048600991124     c     'DPP'         reade     tntbe000                               31
048700991124     c                   enddo
048800160318      *
048900160318     c                   movel(p)  'NSD'         tbeCOD
049000160318     c                   movel(p)  '1'           tbeKE1
049100160318     C     KTBE          chain     tntbe01l
049200160318     c                   if        %Found(tntbe01l)  and TBEatb = *BLANKS
049300160318     c                   eval      dNSD = tbeUNI
049400160318     c                   end
049500160317      *
049600160916      * abilitazione filiali in Partenza e partite tutte
049700160916     c                   movel(p)  'VPO'         tbeCOD
049800160916     c                   movel(p)  'FIEU21R1'    tbeKE1
049900160916     C     KTBE          chain     tntbe01l
050000160916     c                   if        %Found(tntbe01l)  and TBEatb = *BLANKS
050100160916     c                   eval      dvpoFIEU21 = tbeUNI
050200160916     c                   end
050300160916      *
050400160317     c                   z-add     0             vers_CAP_DPD      5 0
050500160317     c* recupero l'ultima versione di cappario DPD
050600160317     c                   clear                   tisie2ds
050700160317     c                   eval      sie2dri=*all'9'
050800160317     c                   call      'TISIE2R'
050900160317     c                   parm                    tisie2ds
051000160317     c* se trovata versione reperisco  descrizioni deposito
051100160317    2c                   if        sie2ver > 0
051200160317     c                   z-add     sie2ver       vers_CAP_DPD
051300160317     c                   end
051400160317      *
051500160317      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
051600160317     c                   movel     kpjbu         svkpjbu
051700160317     C                   CLEAR                   TRUL06DS
051800160317     C                   MOVE      '£1'          D06COD
051900160317     C                   MOVEL     Filiale       D06KEY
052000160317     C                   MOVEL     'L'           D06TLA
052100160317     C                   MOVEL     TRUL06DS      KPJBU
052200160317     C                   CALL      'TRUL06R'
052300160317     C                   PARM                    KPJBA
052400160317     c                   MOVEL     KPJBU         TRUL06DS
052500160317     c                   eval      kpjbu = svkpjbu
052600000908      *
052700971216      * Recupero data e ora
052800971216     C                   TIME                    WORA
052900991124     C                   TIME                    W0140
053000991124      * UDATE IN GGMMAAAA
053100991124     C                   MOVE      W0140         WDTGIO
053200991124      * UDATE IN AAAAMMGG
053300991124     C     *eur          MOVEL     WDTGIO        DATA_eur
053400991124     C     *iso          MOVEL     DATA_eur      dateu
053500160718     C                   MOVEL     dateu         datcor            8 0
053600141103      *-------------
053700160916     C                   MOVE      '999'         all999            3 0
053800160916      *-------------
053900991124     C                   ENDSR
054000060621      * ?------------------------------------------------------------------ */
054100060621      *?      X non bloccare in nessun caso il traduttore CLIENTI
054200060621      * ?------------------------------------------------------------------ */
054300060621     C     *pssr         BEGSR
054400060621     C
054500120504     C*
054600160318     C                   eval      wrkMsg='Creazione Bolla DPD da Spunta VDL h-
054700160318     C                             a avuto problemi e non ha generato la spedi-
054800160318     C                             zione dal FIBARDPD0F con il barecode :'
054900160318     C                             + barecode28 + '.'
055000090605      **    avviso tramite MAIL
055100090605     C                   exSR      eMail_alert
055200090605      **
055300060621     C                   ENDSR     '*CANCL'
055400060621     C
055500090605      * ?------------------------------------------------------------------ */
055600160316      *?  Invio Avviso x problemi nella creazione da spunta DPD
055700090605      * ?------------------------------------------------------------------ */
055800090605     C     eMail_alert   BEGSR
055900090605     C*
056000090605     C                   eval      wrkEml =
056100090605     C                             'Andrea.Bertocchi@bartolini.it'
056200140828     C*
056300160316     C                   eval      wrkMsg = 'Problemi durante la scrittura del -
056400160316     C                             VAB mentre leggeva il FIBARDPD con parcel :'
056500140828     c                             +':/N'+
056600160316     C                             %subst(BARECODE28:9:14)
056700090605     C*
056800160316     C                   eval      wrkOgg='Scrittura VAB da spunta VDL con -
056900160316     C                             (FIBARDPD0F)'
057000090605     C*
057100140828     C                   call(e)   'TRTCT00R2'
057200090605     C                   parm                    wrkEml          253
057300090605     C                   parm                    wrkOgg           44
057400090605     C                   parm                    wrkMsg         5000
057500090605      *
057600090605     C                   eval      wrkEml =
057700090605     C                             'CedAlert@bartolini.it'
057800090605     C*
057900140828     C                   call(e)   'TRTCT00R2'
058000090605     C                   parm                    wrkEml          253
058100090605     C                   parm                    wrkOgg           44
058200090605     C                   parm                    wrkMsg         5000
058300090605      *
058400090605     C                   ENDSR
058500060614      *-------------------------------------------------------------------------
058600160317      *  in base al Depot di provenienza determina Linea e Cliente
058700160317      *----------------------------------------------------------------
058800160317     C     LNP_da_dove   BEGSR
058900160318      **
059000160318      * Default
059100160318      ** aggancia i dati della tab.LPD  di default
059200160318     c                   if          Linea_Partenza = 0
059300160318     c                   z-add     190           Linea_Partenza    3 0
059400160318     c                   end
059500160317      *
059600160317      *  con il DEPOT di provenienza  aggancia x Versione il Cappario DPD
059700160317      *   e se non fosse presente con i primi 4 caratteri del Parcel
059800160317      *
059900160317     C     key_Depot     chain     DPcdp01l
060000160317     c                   if        %Found(DPcdp01l)
060100160317      *
060200160317     C     key_Dep       setgt     DPpdc01l
060300160317     C     DepotParcel   readpe    DPpdc01l
060400160317     c                   dow       not %Eof(DPpdc01l)
060500160317      *   se trovato il Depot VALIDO
060600160317     c                   if        pdcDDE <= dateU and dateU <= pdcDSC
060700160317     c                   eval      DpdcFLO = pdcFLO
060800160317      *
060900160317     c                   if        §PDCFLOLNP <> *blank  and
061000160318     c                             §PDCFLOLNP <> '000'   and
061100160318     c                             %check(digits:§PDCFLOLNP)=0
061200160318     c                   move(p)   §PDCFLOLNP    linea_PARCEL      3 0
061300160318      **
061400160318      **  controlla se appartiene alle linee della £1
061500160318     c     linea_PARCEL  lookup    LIN                                    25
061600160318     c                   if        %Found
061700160317     c                   move      §PDCFLOLNP    linea_partenza    3 0
061800160318     c                   leave
061900160318     c                   end
062000160318      **
062100160317     c                   end
062200160317     c                   end
062300160317      *
062400160317     C     DepotParcel   readpe    DPpdc01l
062500160317     c                   Enddo
062600160317     c                   End
062700160318      *
062800160317     c                   Endsr
062900160317     C*----------------------------------------------------------------
063000160317     C*  Aggancia tab.LPD ed imposta i campi del VAB
063100160317     C*----------------------------------------------------------------
063200160317     C     tabella_LPD   BEGSR
063300160317      *
063400160317     c                   clear                   FIEULPDds
063500160318      *
063600160317     c                   movel(p)  linea_partenzaiLPDlnp
063700160318     c                   movel(p)  linea_partenzaiLPDlna
063800160317     c                   movel(p)  nrParcel      iLPDparcel
063900160318      *
064000160317     c                   call      'FIEULPDR'
064100160317     c                   parm                    FIEULPDds
064200160317      *
064300160317      * ha trovato i dati necessari per instradare il record della spedizione
064400160317      * alla filiale di partenza
064500160318     c                   clear                   dLPD
064600160318     c                   if         oLPDtip <> 'E'
064700160318     c                   eval           dLPD = oLPDUNI
064800160317     C                   eval      su_EDIVAB = §LPDWRK
064900160318     C                   eval         vabCCM = §LPDKSC
065000160318     C                   eval         vabCTM = §LPDCTM
065100160330      *
065200160330      ** tariffa non sarebbe corretta
065300160330     C                   eval         vabCTR = OLPDTAR
065400160318     C                   movel     linea_partenzaVABLNP
065500160318     C                   movel     linea_partenzaVABFGS
065600160317     c                   end
065700160317      *
065800160317      *  QUI la tariffa LHS non deve essere considerata poichè posso conoscere
065900160317      *  la linea di PARTENZA ma la linea di ARRIVO non la conosco.
066000160627      *-------------
066100160627     C                   ENDSR
066200160627      * ?------------------------------------------------------------------ */
066300160627      *?      Scrive il BLP - bolla automatica come VEDI PACCO
066400160627      * ?------------------------------------------------------------------ */
066500160627     C     Scrive_BLP    BEGSR
066600160718     C*
066700160718     C* REPERISCE IL TERMINAL PARTENZA x l'area dati 34DS
066800160718     C                   CLEAR                   FNLV55DS
066900160718     C                   MOVEL     'P'           D55TPT
067000160718     C                   MOVEL     vablnp        D55LIN
067100160718     C                   Z-ADD     DATCOR        D55DRF
067200160718     C                   CALL      'FNLV55R'
067300160718     C                   PARM                    FNLV55DS
067400160718IF  1C     D55ERR        IFNE      *BLANKS
067500160718     C                   MOVEL     vablnp        D55TFP
067600160718E   1C                   ENDIF
067700160718     C*
067800160719      ** imposta il SIMFEL come terminal di partenza
067900160719      *   Aggiorno area dati Dati Utente
068000160719     C     *lock         in        DDatiUte
068100160718     c                   movel     d55tfp        simfel
068200160719     C                   out       DDatiUte
068300160719     C                   UnLock(e) DDatiUte
068400160629      *
068500160627      * altrimentideve se non c'è riuscito, scrive il VAB ed il VAT
068600160627     c                   clear                   dblp
068700160629     C                   MOVEL     vabmgs        §lpmgs                         = oggi
068800160629     c                   movel     vabtsp        §lptsp
068900160629     c                   z-add     vabncl        §lpncl
069000160629     C                   z-add     vabrmn        §lprmn
069100160629     c                   eval      §lpnpr=Nrparcel
069200160629     c                   Movel     vabcad        §LpCad
069300160629     C                   MOVEL     vabprd        §lpprd
069400160629     c                   z-add     vabpkb        §lpPKB
069500161006     c                   z-add     vabvlb        §lpVLB
069600160629     C                   MOVEL     vabcbo        §lpcbo                         NO C/assegno
069700160627      * ?Provenienza
069800160627     c                   move      *all'9'       §lppoe
069900160627     c                   move      *all'9'       §lpnsr
070000160627     c                   move      *all'9'       §lpnor
070100160627     c                   move      *all'9'       §lpnrv
070200160629      * ?SE aveva un ORM legato.
070300160627     C                   if         §lpcbo = '2'  and dpoPOE > 0
070400160629     C                   z-add     dpoPOE        §LPPOE
070500160629     C                   z-add     dpoNRS        §LPNSR
070600160629     C                   z-add     dpoNOR        §LPNOR
070700160629     C                   z-add     dpoNRV        §LPNRV
070800160629     c                   eval      §lpffd = vabffd
070900160629     c                   end
071000160630     c                   z-add     vabaas        §lpaas
071100160718     c                   z-add     vablnp        §lplnp
071200160629     c                   movel     vabfgs        §lpfgs
071300160629     c                   movel     vabRSD        §lpRSD
071400160629     c                   movel     vabIND        §lpIND
071500160629     c                   movel     vabLOD        §lpLOD
071600160630     c                   z-add     vabccm        §lpccm
071700160630     c                   z-add     vabccm        §lpccn
071800160627      *
071900160627     c* per il momento imposto ccn = ccm
072000160627     c                   eval      §lpdrt=dateu
072100160627     c                   eval      §LPFPP='P'
072200160629     c                   eval      §lpfpr='D'
072300160629     c                   eval      §lpsop=':'
072400160627     c                   clear                   dblp01
072500160627     c                   eval      §LPscd=ServCode
072600160627     c                   movel     '1'           §lptrd
072700160627     c                   movel     dblp01        §lpflo
072800160920      **  L'Autista
072900160920     c* il padroncino è da prendere da tb bsp: altrimenti imposto fisso 999
073000160920     c     §lplnp        chain     fibsp01l
073100160920     c                   if        %found(fibsp01l)
073200160920     c                   z-add     bspPDR        §lppdr
073300160920     c                   else
073400160920     c                   z-add     999           §lppdr
073500160920     c                   end
073600160920      *
073700160629     c                   movel     §lplnp        §lppdr
073800160629      *
073900160629      ******  DA SOSTITUIRE PRENDENDOLO DALLA TABELLA
074000160630     c                   eval      §lpCTM= §nsdCTM
074100160629     C                   clear                   DTASV
074200160629     C                   clear                   trul90ds
074300160629      ******
074400160916     C                   CALL      'FNLS01R'                            99
074500160627     C                   PARM                    KPJBA
074600160627     C                   PARM                    DBLP
074700160627     C                   PARM                    DTASV
074800160627     C                   PARM                    trul90ds
074900160627      *
075000160629      *  Se non c'è riuscito a scrivere la bolla
075100160916     c                   if        §LPFPR='5' or *in99
075200160629     c                   move      'S'           scrivereVAB
075300160629     c                   end
075400160317      *
075500160317     c                   Endsr
075600160718      *---------------------------------------------------------------*
075700160718      *?  Reperimento Dati del job (Utente/Operativi)                ?
075800160718      *---------------------------------------------------------------*
075900160718     c     DatiJOB       BEGSR
076000160718      *
076100160718     c     *dtaara       define    §azute        azuteds
076200160718     c     *dtaara       define    §datiute      ddatiute
076300160718      *
076400160719     c     *lock         in(E)     *dtaara
076500160718     c                   IF        %ERROR or RSUT = *blanks
076600160718     c                   clear                   Tibs34Ds
076700160718     c                   call      'TIBS34R'
076800160718     c                   parm                    Tibs34Ds
076900160718     c                   in        *dtaara
077000160718     c                   ENDIF
077100160718      *
077200160718     c                   ENDSR
077300160718     C*---------------------------------------------------------------*
