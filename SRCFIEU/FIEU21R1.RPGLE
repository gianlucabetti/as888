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
002700171011      *
002800050414     FFiVAB01L  UF A E           K DISK
002900050414     FEDivab3L  UF A E           K DISK
003000041215      *
003100050414     FEDiVAT3L  Uf A E           K DISK
003200050414     FFiVAT01L  Uf A E           K DISK
003300160317      *----------------------------------------------------*
003400160317     D TRUL06DS      E DS
003500160317     D  LIN                    1     90  0 DIM(30)
003600940321      *----------------------------------------------------*
003700991124      * Schiere per conversione peso DPD
003800991124     D DPPK            S             15    DIM(30)                              CODICE NAZIONE
003900991124     D DPP             S              7S 1 DIM(30)                              CODICE NAZIONE
004000141210     D VLB             S              5S 3 DIM(30)                              CODICE NAZIONE
004100160629      *-------------------
004200160629     D DBLP01        E DS
004300160629     D DBLP          E DS
004400160629     D DTASV         E DS
004500160629     D trul90ds      E DS
004600160916     D DVPOFIEU21    E DS
004700160916     D  VPO21R1                1     60  0 DIM(20)
004800160317      *-------------------
004900160317     D KPJBA         E DS
005000160317     D OG143         E DS
005100160317     D DDPP          E DS
005200160317     D DLPD          E DS
005300160318     D DNSD          E DS
005400160317     D FIEULPDDS     E DS
005500160718      *--------------------------
005600160718     d* Parametri x Controllo profilo utenti
005700160718     d TIBS34ds      e ds
005800160718     d AZUTEds       e ds                  extname(AZUTE00F)
005900160718     d DDatiUte      e ds
006000160718     D*------------------
006100160718     D* DS REPEIMENTO TERMINALs
006200160718     D*------------------
006300160718     D FNLV55DS      E DS
006400171011      ** ------------------------------------------------------------------ */
006500171011     D prmCPD10F     e DS                  extname(DPCDP10F)
006600171011     D prmLEG10F     e DS                  extname(DPLEG10F)
006700171011     D TisieDEPds    E DS
006800171011      ** ------------------------------------------------------------------ */
006900171011     D DlegFLO       E DS
007000160718      *--------------------------
007100991129     D W015A           S             15
007200091109     D W030A           S             30
007300000223     D W0140           S             14  0
007400991129     D WORA            S              6  0
007500991129     D XX              S              3  0 INZ
007600991129     D WDTGIO          S              8  0
007700991129     D DATEU           S              8  0
007800991129     D DATA_eur        S               D   DATFMT(*eur)
007900050112     D svkpjbu         s                   like(kpjbu)
008000060608      **?------------------------------------------------------------------ */
008100060612     D DVate         E DS
008200060620      *
008300160316     D DsBareCode      DS
008400160316     d  Percent                1      1
008500160316     d  CAPdest                2      8
008600160316     D  DepotParcel            9     12
008700160316     d  NrParcel               9     22
008800160316     d  ServCode              23     25
008900160316     d  Nazione               26     28
009000060608      **?------------------------------------------------------------------ */
009100050112      * Numeratore Bolle (302)
009200050112     D trul33ds      E DS
009300050112     D Ds3C          E DS
009400070202     D TISI95DS      E DS
009500160317     D KAAA            S                   LIKE(NUFAAA)
009600160317     D KCNU            S                   LIKE(NUFCNU)
009700160317     D KFIL            S                   LIKE(NUFFIL)
009800160317     D NUM_Sped        s                   LIKE(vabnsp)
009900060612      * ?   *--------------------------------------------------------------*
010000140827      **
010100060710     D Digits          C                   '0123456789'
010200160317     d Normal_Parcel   c                   '101'
010300160317     d Small_Parcel    c                   '136'
010400160317     d Return_Sender   c                   '298'
010500160317     d Return          c                   '300'
010600160317     d Gomme_365       c                   '365'
010700160317     d Gomme_366       c                   '366'
010800160317     d Gomme_367       c                   '367'
010900111110     d Normal_Parcel_327...
011000160317     d                 c                   '327'
011100111110     d Normal_Parcel_329...
011200160317     d                 c                   '329'
011300111110     d Small_Parcel_328...
011400160317     d                 c                   '328'
011500060612      * ?================================================================== */
011600160316     c                   SETON                                        LR
011700160316     c                   eval      DsBareCode = barecode28
011800160316      **
011900160316      **  Reso NON si scrive
012000160317     c                   if        ServCode  = Return_Sender
012100160317     c                               or
012200160317     c                             ServCode  = Return
012300160316     c                               or
012400160317     c                             DepotParcel = '0844'
012500160317     c                               or
012600160317     c                             DepotParcel = '0845'
012700160318      *
012800160318      * abilitato significa anche abilitato alla cancellazione del record
012900160318      *   poichè è riuscito a impostare il record bolla quindi può cancellare la spia
013000160318     c                   MOVE      'S'           abilitato         1
013100160317      * ?            ==============
013200160316     c                   return
013300160317      * ?            ==============
013400160316     c                   ENDIF
013500160317
013600160316      **?------------------------------------------------------------------ */
013700160316      * Clearizzo EDiVAB e FiVAB
013800160316     c                   clear                   edivab00
013900160316     c                   clear                   fivab000
014000160316     c                   clear                   su_EDIVAB         1
014001171123     c                   clear                   w_daorm           1
014100060612      **?------------------------------------------------------------------ */
014200160316      * ?Data  spedizione
014300160721     C                   MOVE      datcor        VABMGS                         = oggi
014400160317      * ?Tipo servizio/Colli
014500160316     c                   movel     'C'           vabtsp
014600160316     c                   z-add     1             vabncl
014700160721     C                   MOVEL     DATCOR        CAMPO4            4
014800160721     C                   MOVE      CAMPO4        VABAAS
014900160316     c
015000160316      * ?il Parcel deve essere impostato nella DS VATE
015100160316      * fisso il rif.numerico a (1) e sostituito se contiene solo numeri
015200160316     C                   z-add     1             vabrmn
015300160317     c     digits        check     NrParcel      position          3 0
015400160316      **  presenti campi non numerici: (Ossia trovati)
015500160606     c                   iF         %Found
015600160606     c                   MOVE      'S'           abilitato         1
015700160606     c                   RETURn
015800160606     c                   else
015900160606      * ?  per impostare il riferimento numerico (VABRMN):
016000160606     C                   move      NrParcel      vabrmn
016100160606     c                   enD
016200160317      * ?CAP
016300160318     c                   move      CAPdest       field5            5            C.A.P.
016400160318     c                   movel     field5        vabcad                         C.A.P.
016500160316      * ?Se manca la provincia la calcolo
016600160316     C                   CLEAR                   TISI95DS
016700160316     C                   MOVEL     vabCAD        I95CAP
016800160316     C                   MOVEL     DATEU         I95DAT
016900160316     C                   MOVEL     '3'           I95TCN
017000160316     C                   CALL      'TISI95R'
017100160316     C                   PARM                    TISI95DS
017200160316     C     O95ERR        IFeq      *BLANKS
017300160316     C                   MOVEL     O95PRV        vabPRD
017400160329     C                   Else
017500160329      * x errore NON scrive la spedizione
017600160329     C                   RETURN
017700160329     C                   END
017800160316      *
017900160316      ** imposta il Depot di provenienza x testare se VTG
018000160316     c                   clear                   parcel_VTG        1
018100160316     C                   movel(p)  'VTG'         cldCOD
018200160316     C                   move      DepotParcel   cldDEP
018300160316     c     key_VTG       setll     tncld00F
018400160316     c     key_VTG       reade     tncld00F
018500160316     c                   dow       not %EoF(tncld00F)
018600160316     c                   if        dateu >= cldDDA and
018700160316     c                             dateu <= cldADA
018800160316     c                   move      'S'           parcel_VTG
018900160316     c                   leave
019000160316     c                   end
019100160316     c     key_VTG       reade     tncld00F
019200160316     c                   endDO
019300160317      * ?Service  Code:
019400160316     c                   clear                   w015A
019500160317     c                   if        ServCode  = Normal_Parcel
019600160316     c                               or
019700160317     c                             ServCode  = Normal_Parcel_327
019800160317     c                               or
019900160317     c                             ServCode  = Normal_Parcel_329
020000160316     c                   eval      W015A = 'PESO01'
020100160316     c                   end
020200160317      **  traffico gomme.
020300160317     c                   if        ServCode  = Gomme_365
020400160316     c                               or
020500160317     c                             ServCode  = Gomme_366
020600160316     c                               or
020700160317     c                             ServCode  = Gomme_367
020800160316     c                   eval      W015A = 'PESO03'
020900160316     c                   end
021000160317      * piccolo
021100160317     c                   if        ServCode  = Small_Parcel
021200160316     c                               or
021300160317     c                             ServCode  = Small_Parcel_328
021400160316     c                   eval      W015A = 'PESO02'
021500160316     c                   end
021600160316      * se per qualche motivo non passano il codice pacco/peso imposto come
021700160316      *   pacco grande
021800160316     c                   if        W015A = *blank
021900160316     c                   eval      W015A = 'PESO01'
022000160316     c                   end
022100160316      *
022200160316      * ?Peso, ricavato dal tipo servizio del ROUTELABEL
022300160316     c                   z-add     1             xx
022400160316     c                   if        w015a <> *blank
022500160316     c     W015A         lookup    DPPK(xx)                               31
022600160316     c                   If        *in31 = *off
022700160316     c                   eval      W015A = 'PESO'
022800160316     c     W015A         lookup    DPPK(xx)                               31
022900160316     c                   Endif
023000160316     c                   z-add     DPP(xx)       vabPKB
023100161006     c                   z-add     vlb(xx)       vabVLB
023200160316     c                   End
023300160316      * ?Codice bolla
023400160316     C                   MOVEL     '1'           vabcbo                         NO C/assegno
023500160317      * ?Provenienza
023600160317     c                   movel     '1'           VABscl
023700160316      *
023800160316      * ?Controlla esistenza del Parcel sul file degli ORM dati a DPD per
023900160317      * ?assegnato ORM
024000160317     C     nrParcel      chain     fidpo06l
024100160316     c                   if        %Found(fidpo06l)
024200160316     C                   MOVEL     '2'           vabcbo
024300160316     c                   end
024400160316      *
024500160316      * ?SE si tratta di un ORM commissionato a DPD, Controllo se deve essere
024600160316      * ?in FERMO DEPOSITO.
024700160316     c                   eval      vabFFD = *blank
024800160316     C                   if         vabcbo = '2'  and dpoPOE > 0
024900160316     C                   z-add     dpoPOE        ORMPOE
025000160316     C                   z-add     dpoNRS        ORMNSR
025100160316     C                   z-add     dpoNOR        ORMNOR
025200160316     C                   z-add     dpoNRV        ORMNRV
025300160317      * ?sull'ORM se Fermo Deposito
025400160316     C     KORM          chain     fnORM01l
025500171123     c****               if        %Found(FnOrm01l) and ormffd ='S'
025501171123     c                   if        %Found(FnOrm01l)
025502171123     c                   if        ormffd = 'S'
025600160316     c                   eval      vabFFD = 'S'
025601171123     c                   endif
025602171201     c* Prendo dall'orm anche il cap destinatario tanto se fosse diverso dal cap presente
025603171201     c* sull'etichetta positrova esegue sempre due tentativi di ricerca:
025604171201     c* prima con il barocde completo poi solo con il chi sono
025605171123     c                   if        ormrsc<>*blanks
025606171123     c                   movel     ormrsc        vabrsd
025607171123     c                   movel     orminc        vabind
025608171123     c                   movel     ormloc        vablod
025609171201     c                   movel     ormCAC        vabCAD
025610171201     c                   movel     ormprc        vabprd
025611171123     c                   eval      w_daorm = 'S'
025612171123     c                   endif
025700160316     c                   end
025800160316     c                   end
025900160317      **
026000160317      *--------
026100160317     C* con la Linea di chi spara prende quella DPD da sostituire su LNP e FGS
026200160317     c     Filiale       chain     azORG01l
026300160317     c                   if          %Found(azORG01l)
026400160317     c                   movel     orgde3        OG143
026500160317     c                   if           §OGLNPB <> *blank and §OGLNPB <> '000'
026600160318     c                   move      §OGLNPB       VABLNP
026700160318     c                   move      §OGLNPB       VABFGS
026800160318     c                   move      §OGLNPB       Linea_Partenza    3 0
026900160317     c                   end
027000160317     c                   end
027100160318      **
027200160318      **  ricava la LNP e altri valori fondamentali dal PARCEL/DEPOT di Partenza
027300160318     c                   exsr      LNP_da_dove
027400160318      *
027500160318      ** tab.definizione dati x Linea DPD
027600160318     c                   exsr      tabella_LPD
027700160317      *
027800171123      * da tabella NSD (1)  solo se non li prendo da ORM
027801171123     c                   if        w_daorm = *blanks
027900160318     c                   movel     §NSDRSD       VABRSD
028000160318     c                   movel     §NSDIND       VABIND
028100160318     c                   movel     §NSDLOD       VABLOD
028101171123     c                   endif
028200160318      *
028300160317      * abilitato significa anche abilitato alla cancellazione del record
028400160317      *   poichè è riuscito a impostare il record bolla quindi può cancellare la spia
028500160317     c                   MOVE      'S'           abilitato         1
028600160916      *
028700160916     c                   move      'N'           scrivereVAB       1
028800160916      *
028900160916      * Controlla se VPO attivata sulla filiale o su tutte
029000160916     c     Filiale       lookup    vpo21r1                                21
029100160916     c  n21all999        lookup    vpo21r1                                21
029200160916     c  n21              eval          scrivereVAB ='S'
029300160916      *
029400160916      *  Se presente in VPO o tutto attivo "999"
029500060612      * -----------------                  -------------------
029600160720      * ?scrive SPED.VEDI PACCO in PARTENZA BLP
029700160916     C   21              EXSR      Scrive_BLP
029800160720      * -----------------                  -------------------
029900160627      *
030000151113      * ?==============================
030100060612      * ?Scrivo EDiVAB o FiVAB
030200151113      * ?==============================
030300160629     c                   if            scrivereVAB ='S'
030400160720      * -----------------                  -------------------
030500160720      * ?Numero spedizione
030600160720     C                   EXSR      SUB_keybol
030700160720      * -----------------                  -------------------
030800160317     C                   IF        su_EDIVAB = 'S'
030900160317     c                   eval      vabcmr = 'GEO_' + %editc(dateu:'Y') + '_PST'
031000060612     c                   z-add     DATEU         VABdcm
031100060612     c                   z-add     DATEU         VABdts
031200060612     c                   movel     WORA          VABhms
031300060612     c                   write     EDivab00
031400060612     c                   ELSE
031500060612     c                   write     FiVAB000
031600060612     c                   ENDIF
031700060614      ***
031800160316     c                   exsr      wrtvat
031900160720     c                   end
032000060612      *
032100160317     c                   Return
032200060612     C*----------------------------------------------------------------
032300060612     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
032400060612     C*----------------------------------------------------------------
032500060612     C     WRTVAT        BEGSR
032600060612      *
032700060413      * ? Parcel Nr. --> Riferimento Parcel DPD
032800051219     C                   If        su_EDIVAB = 'S'
032900041215     c                   clear                   edivat00
033000041215     c                   else
033100041215     c                   clear                   fivat000
033200041215     c                   end
033300160316      **  su nuovo tipo record (E)
033400160316     C                   MOVEL     'E'           VATTRC
033500041215     C                   movel     VABfgs        VATfgs
033600041215     C                   Z-ADD     VABCCM        VATCCM
033700041215     C                   Z-ADD     VABAAS        VATAAS
033800041215     C                   Z-ADD     VABLNP        VATLNP
033900041215     C                   Z-ADD     VABNRS        VATNRS
034000041215     C                   Z-ADD     VABNSP        VATNSP
034100060621      *
034200060621      *  IMPOSTA la DS VAT(E)
034300160316     c                   clear                   DVate
034400160316     C                   MOVEL     NrParcel      §VATEPRN
034500060620     C                   move      *zeros        §VATESCD
034600160316     C                   move      ServCode      §VATESCD
034700160316     C                   move(p)   '%'           §VATEBID
034800060620      *
034900171011     c                   movel     I_DEPOT7      §VATESDEP
035000060608     C                   MOVEL     DVate         VATNOT
035100041215      **
035200051219     C                   If        su_EDIVAB = 'S'
035300041215     C                   MOVEL     VABCMR        VATCMR
035400041215     C                   Z-ADD     VABCNT        VATCNT
035500041215     C                   WRITE     EDiVAT00
035600041215     c                   else
035700041215     C                   WRITE     fiVAT000
035800041215     c                   end
035900060413      *
036000041215     C                   ENDSR
036100971215      *----------------------------------------------------------------
036200971216      * DETERMINO DATA E CHIAVE SPEDIZIONE
036300971215      *----------------------------------------------------------------
036400991206     C     SUB_keybol    BEGSR
036500050112      *
036600050112      **              **------------------**
036700050112      *    deve controllare sulla tabella "3C" se il numero spedizione
036800050112      *     deve essere mantenuto oppure no
036900050112     C                   clear                   Ds3C
037000060620     C                   Z-ADD     1             TBLKUT
037100050112     C                   MOVEL     '3C'          TBLCOD
037200050112     C                   movel(p)  VABccm        TBLKEY
037300160317     C     KTABel        CHAIN     TABEL00F
037400050112     C                   IF        %Found(TABEL00F) and tblflg = *blank
037500050112     C                   MOVEL     TBLUNI        Ds3C
037600050112     C                   END
037700050112      *
037800050112      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
037900050112     c                   if        §3CFsp <> 'S'
038000050112      *
038100050112     C* NSP => Stacco un numeratore da AZNUM
038200050112     c                   movel     kpjbu         svkpjbu
038300050112     c                   clear                   kpjbu
038400050112     C                   clear                   TRUL33DS
038500050112     C                   eval      I33OPE = *zeros
038600050112     C                   eval      I33CNU = 302
038700050112     C                   eval      I33NUM = 1
038800050112     C                   movel     TRUL33DS      KPJBU
038900050112     C                   call      'TRUL33R'
039000050112     C                   parm                    KPJBA
039100050112     C                   movel     KPJBU         TRUL33DS
039200050112     C                   if        O33ERR = *zeros
039300050112     c                   z-add     o33nrf        NUM_SPED
039400050112     C                   endif
039500050112     c                   movel     svkpjbu       kpjbu
039600050112      ****
039700050112     c                   ELSE
039800050112      *   se si vuole mantenere il numero spedizione
039900050112      ****
040000160721     C                   MOVEl     datCOR        KAAA
040100050112     C                   Z-ADD     3             KCNU
040200050112     C                   Z-ADD     VABLNP        KFIL
040300050112     C     KNUF          CHAIN     FLNUF                              9999
040400050112     C                   IF        *IN99 = *OFF
040500050112     C                   ADD       1             NUFNUM
040600050112     C                   Z-ADD     NUFNUM        NUM_Sped                       *NUM.SPEDIZIONE
040700050112     C                   UPDATE    FLNUF
040800060612     c                   END
040900060411      *
041000050112     c                   EndIF
041100050112      **
041200050112      **              **------------------**
041300050112     C                   Z-ADD     NUM_Sped      VABNSP                         *NUM.SPEDIZIONE
041400050112      **              **------------------**
041500160721     C                   MOVEL     DATCOR        CAMPO4            4
041600160721     C                   MOVE      CAMPO4        KAAA
041700160721     C                   MOVE      CAMPO4        VABAAS
041800971216      *
041900991206     C                   ENDSR
042000051205      *----------------------------------------------------*
042100051205      *   DEFINIZIONE CHIAVI                               *
042200051205      *----------------------------------------------------*
042300051205     C     *INZSR        BEGSR
042400051205      *
042500991129     c     *ENTRY        PLIST
042600160317     C                   parm                    Filiale           3 0
042700160316     C                   parm                    BareCode28       28
042800160316     C                   parm                    Abilitato         1
042900160719     C                   parm                    kpjba
043000160316      *
043100160718      *  prende i dati utente
043200160718     c                   exsr      DATIjob
043300160718      *
043400160316      *  se scriverà correttamente il VAB deve impostare "S" in abilitato
043500160316      *  che significa che il programma chiamante può cancellare la spunta
043600160316      *  poichè già utilizzata.
043700991129      *
043800991206     C     KNUF          KLIST
043900991206     C                   KFLD                    KAAA
044000991206     C                   KFLD                    KCNU
044100991206     C                   KFLD                    KFIL
044200971215      *
044300050112     C     KTABel        KLIST
044400050112     C                   KFLD                    TBLKUT
044500050112     C                   KFLD                    TBLCOD
044600050112     C                   KFLD                    TBLKEY
044700160318      *
044800160318     C     KTBE          KLIST
044900160318     C                   KFLD                    TBECOD
045000160318     C                   KFLD                    TBEKE1
045100100920      *
045200100920     C     KORM          KLIST
045300100920     C                   KFLD                    ORMPOE
045400100920     C                   KFLD                    ORMNSR
045500100920     C                   KFLD                    ORMNOR
045600100920     C                   KFLD                    ORMNRV
045700150604      *
045800150604     C     key_VTG       KLIST
045900150604     C                   KFLD                    cldCOD
046000150604     C                   KFLD                    cldDEP
046100160301      **
046200050414      *------------------
046300020919      * Apro file transito o FiVAB pilotato dalla tabella
046400991124      * Caricamento Tabella PESI DPD
046500060620     c                   clear                   XX
046600991124     c                   clear                   DPP
046700991124     c     'DPP'         chain     tntbe000                           31
046800991124      *
046900991124     c                   DOW       *in31 = *off
047000991124      * Se il S.I. è indicato deve essere uguale al mio
047100991124     c                   IF        TBEatb = *BLANKS  and
047200991129     c                              (TBEsif = Knsif  or  TBEsif = *blanks)
047300991124     c                   add       1             xx
047400991124     c                   movel     TBEke1        DPPK(XX)
047500000406     c                   movel     TBEuni        DDPP
047600000410     c                   movel     §DPPpkb       DPP(XX)
047700141210     c                   movel     §DPPvlb       vlb(XX)
047800991124     c                   endif
047900991124     c     'DPP'         reade     tntbe000                               31
048000991124     c                   enddo
048100160318      *
048200160318     c                   movel(p)  'NSD'         tbeCOD
048300160318     c                   movel(p)  '1'           tbeKE1
048400160318     C     KTBE          chain     tntbe01l
048500160318     c                   if        %Found(tntbe01l)  and TBEatb = *BLANKS
048600160318     c                   eval      dNSD = tbeUNI
048700160318     c                   end
048800160317      *
048900160916      * abilitazione filiali in Partenza e partite tutte
049000160916     c                   movel(p)  'VPO'         tbeCOD
049100160916     c                   movel(p)  'FIEU21R1'    tbeKE1
049200160916     C     KTBE          chain     tntbe01l
049300160916     c                   if        %Found(tntbe01l)  and TBEatb = *BLANKS
049400160916     c                   eval      dvpoFIEU21 = tbeUNI
049500160916     c                   end
049600160317      *
049700160317      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
049800160317     c                   movel     kpjbu         svkpjbu
049900160317     C                   CLEAR                   TRUL06DS
050000160317     C                   MOVE      '£1'          D06COD
050100160317     C                   MOVEL     Filiale       D06KEY
050200160317     C                   MOVEL     'L'           D06TLA
050300160317     C                   MOVEL     TRUL06DS      KPJBU
050400160317     C                   CALL      'TRUL06R'
050500160317     C                   PARM                    KPJBA
050600160317     c                   MOVEL     KPJBU         TRUL06DS
050700160317     c                   eval      kpjbu = svkpjbu
050800000908      *
050900971216      * Recupero data e ora
051000971216     C                   TIME                    WORA
051100991124     C                   TIME                    W0140
051200991124      * UDATE IN GGMMAAAA
051300991124     C                   MOVE      W0140         WDTGIO
051400991124      * UDATE IN AAAAMMGG
051500991124     C     *eur          MOVEL     WDTGIO        DATA_eur
051600991124     C     *iso          MOVEL     DATA_eur      dateu
051700160718     C                   MOVEL     dateu         datcor            8 0
051800141103      *-------------
051900160916     C                   MOVE      '999'         all999            3 0
052000160916      *-------------
052100991124     C                   ENDSR
052200060621      * ?------------------------------------------------------------------ */
052300060621      *?      X non bloccare in nessun caso il traduttore CLIENTI
052400060621      * ?------------------------------------------------------------------ */
052500060621     C     *pssr         BEGSR
052600060621     C
052700120504     C*
052800160318     C                   eval      wrkMsg='Creazione Bolla DPD da Spunta VDL h-
052900160318     C                             a avuto problemi e non ha generato la spedi-
053000160318     C                             zione dal FIBARDPD0F con il barecode :'
053100160318     C                             + barecode28 + '.'
053200090605      **    avviso tramite MAIL
053300090605     C                   exSR      eMail_alert
053400090605      **
053500060621     C                   ENDSR     '*CANCL'
053600060621     C
053700090605      * ?------------------------------------------------------------------ */
053800160316      *?  Invio Avviso x problemi nella creazione da spunta DPD
053900090605      * ?------------------------------------------------------------------ */
054000090605     C     eMail_alert   BEGSR
054100090605     C*
054200090605     C                   eval      wrkEml =
054300090605     C                             'Andrea.Bertocchi@bartolini.it'
054400140828     C*
054500160316     C                   eval      wrkMsg = 'Problemi durante la scrittura del -
054600160316     C                             VAB mentre leggeva il FIBARDPD con parcel :'
054700140828     c                             +':/N'+
054800160316     C                             %subst(BARECODE28:9:14)
054900090605     C*
055000160316     C                   eval      wrkOgg='Scrittura VAB da spunta VDL con -
055100160316     C                             (FIBARDPD0F)'
055200090605     C*
055300140828     C                   call(e)   'TRTCT00R2'
055400090605     C                   parm                    wrkEml          253
055500090605     C                   parm                    wrkOgg           44
055600090605     C                   parm                    wrkMsg         5000
055700090605      *
055800090605     C                   eval      wrkEml =
055900090605     C                             'CedAlert@bartolini.it'
056000090605     C*
056100140828     C                   call(e)   'TRTCT00R2'
056200090605     C                   parm                    wrkEml          253
056300090605     C                   parm                    wrkOgg           44
056400090605     C                   parm                    wrkMsg         5000
056500090605      *
056600090605     C                   ENDSR
056700060614      *-------------------------------------------------------------------------
056800160317      *  in base al Depot di provenienza determina Linea e Cliente
056900160317      *----------------------------------------------------------------
057000160317     C     LNP_da_dove   BEGSR
057100160318      **
057200160318      * Default
057300160318      ** aggancia i dati della tab.LPD  di default
057400160318     c                   if          Linea_Partenza = 0
057500160318     c                   z-add     190           Linea_Partenza    3 0
057600160318     c                   end
057700171011      *
057800171011     c                   clear                   tisieDEPds
057900171011     c                   clear                   prmCPD10F
058000171011     c                   clear                   prmLEG10F
058100171011      *
058200171011      *  con il DEPOT di provenienza  aggancia x Versione il Cappario DPD
058300171011      *   e se non fosse presente con i primi 4 caratteri del Parcel
058400171011     c                   movel     DepotParcel   I_DEPOT4
058500171011      ** ------
058600171011      *  aggancia con il DRIVER i legami
058700171011     c                   call      'TISIEDEPR'
058800171011     c                   parm                    tisieDEPds
058900171011     c                   parm                    prmCPD10F
059000171011     c                   parm                    prmLEG10F
059100171011      ** ------
059200171011      ** -  se ritornato tutto OK
059300171011     c                   if        O_FLGVERS ='0' and O_FLGCAPP ='0' and
059400171011     c                             O_FLGLEGA ='0'
059500160317      *   se trovato il Depot VALIDO
059600171011     c                   eval      DlegFLO = legFLO
059700160317      *
059800171011     c                   if        §legFLOLNP <> *blank  and
059900171011     c                             §legFLOLNP <> '000'   and
060000171011     c                             %check(digits:§legFLOLNP)=0
060100171011     c                   move(p)   §legFLOLNP    linea_PARCEL      3 0
060200160318      **
060300160318      **  controlla se appartiene alle linee della £1
060400160318     c     linea_PARCEL  lookup    LIN                                    25
060500160318     c                   if        %Found
060600171011     c                   move      §legFLOLNP    linea_partenza    3 0
060700160318     c                   end
060800160318      **
060900160317     c                   end
061000171011     c                   endIF
061100160318      *
061200160317     c                   Endsr
061300160317     C*----------------------------------------------------------------
061400160317     C*  Aggancia tab.LPD ed imposta i campi del VAB
061500160317     C*----------------------------------------------------------------
061600160317     C     tabella_LPD   BEGSR
061700160317      *
061800160317     c                   clear                   FIEULPDds
061900160318      *
062000160317     c                   movel(p)  linea_partenzaiLPDlnp
062100160318     c                   movel(p)  linea_partenzaiLPDlna
062200160317     c                   movel(p)  nrParcel      iLPDparcel
062300160318      *
062400160317     c                   call      'FIEULPDR'
062500160317     c                   parm                    FIEULPDds
062600160317      *
062700160317      * ha trovato i dati necessari per instradare il record della spedizione
062800160317      * alla filiale di partenza
062900160318     c                   clear                   dLPD
063000160318     c                   if         oLPDtip <> 'E'
063100160318     c                   eval           dLPD = oLPDUNI
063200160317     C                   eval      su_EDIVAB = §LPDWRK
063300160318     C                   eval         vabCCM = §LPDKSC
063400160318     C                   eval         vabCTM = §LPDCTM
063500160330      *
063600160330      ** tariffa non sarebbe corretta
063700160330     C                   eval         vabCTR = OLPDTAR
063800160318     C                   movel     linea_partenzaVABLNP
063900160318     C                   movel     linea_partenzaVABFGS
064000160317     c                   end
064100160317      *
064200160317      *  QUI la tariffa LHS non deve essere considerata poichè posso conoscere
064300160317      *  la linea di PARTENZA ma la linea di ARRIVO non la conosco.
064400160627      *-------------
064500160627     C                   ENDSR
064600160627      * ?------------------------------------------------------------------ */
064700160627      *?      Scrive il BLP - bolla automatica come VEDI PACCO
064800160627      * ?------------------------------------------------------------------ */
064900160627     C     Scrive_BLP    BEGSR
065000160718     C*
065100160718     C* REPERISCE IL TERMINAL PARTENZA x l'area dati 34DS
065200160718     C                   CLEAR                   FNLV55DS
065300160718     C                   MOVEL     'P'           D55TPT
065400160718     C                   MOVEL     vablnp        D55LIN
065500160718     C                   Z-ADD     DATCOR        D55DRF
065600160718     C                   CALL      'FNLV55R'
065700160718     C                   PARM                    FNLV55DS
065800160718IF  1C     D55ERR        IFNE      *BLANKS
065900160718     C                   MOVEL     vablnp        D55TFP
066000160718E   1C                   ENDIF
066100160718     C*
066200160719      ** imposta il SIMFEL come terminal di partenza
066300160719      *   Aggiorno area dati Dati Utente
066400160719     C     *lock         in        DDatiUte
066500160718     c                   movel     d55tfp        simfel
066600160719     C                   out       DDatiUte
066700160719     C                   UnLock(e) DDatiUte
066800160629      *
066900160627      * altrimentideve se non c'è riuscito, scrive il VAB ed il VAT
067000160627     c                   clear                   dblp
067100160629     C                   MOVEL     vabmgs        §lpmgs                         = oggi
067200160629     c                   movel     vabtsp        §lptsp
067300160629     c                   z-add     vabncl        §lpncl
067400160629     C                   z-add     vabrmn        §lprmn
067500160629     c                   eval      §lpnpr=Nrparcel
067600160629     c                   Movel     vabcad        §LpCad
067700160629     C                   MOVEL     vabprd        §lpprd
067800160629     c                   z-add     vabpkb        §lpPKB
067900161006     c                   z-add     vabvlb        §lpVLB
068000160629     C                   MOVEL     vabcbo        §lpcbo                         NO C/assegno
068100160627      * ?Provenienza
068200160627     c                   move      *all'9'       §lppoe
068300160627     c                   move      *all'9'       §lpnsr
068400160627     c                   move      *all'9'       §lpnor
068500160627     c                   move      *all'9'       §lpnrv
068600160629      * ?SE aveva un ORM legato.
068700160627     C                   if         §lpcbo = '2'  and dpoPOE > 0
068800160629     C                   z-add     dpoPOE        §LPPOE
068900160629     C                   z-add     dpoNRS        §LPNSR
069000160629     C                   z-add     dpoNOR        §LPNOR
069100160629     C                   z-add     dpoNRV        §LPNRV
069200160629     c                   eval      §lpffd = vabffd
069300160629     c                   end
069400160630     c                   z-add     vabaas        §lpaas
069500160718     c                   z-add     vablnp        §lplnp
069600160629     c                   movel     vabfgs        §lpfgs
069700160629     c                   movel     vabRSD        §lpRSD
069800160629     c                   movel     vabIND        §lpIND
069900160629     c                   movel     vabLOD        §lpLOD
070000160630     c                   z-add     vabccm        §lpccm
070100160630     c                   z-add     vabccm        §lpccn
070200160627      *
070300160627     c* per il momento imposto ccn = ccm
070400160627     c                   eval      §lpdrt=dateu
070500160627     c                   eval      §LPFPP='P'
070600160629     c                   eval      §lpfpr='D'
070700160629     c                   eval      §lpsop=':'
070800160627     c                   clear                   dblp01
070900160627     c                   eval      §LPscd=ServCode
070901171123     c* se prendo i dati da ORM considero com VEDI PACCO già manutenzionato in Partenza
070902171123     c                   if        w_daorm = 'S'
070903171123     c                   movel     '3'           §lptrd
070904171123     c                   else
071000160627     c                   movel     '1'           §lptrd
071001171123     c                   endif
071100160627     c                   movel     dblp01        §lpflo
071200160920      **  L'Autista
071300160920     c* il padroncino è da prendere da tb bsp: altrimenti imposto fisso 999
071400160920     c     §lplnp        chain     fibsp01l
071500160920     c                   if        %found(fibsp01l)
071600160920     c                   z-add     bspPDR        §lppdr
071700160920     c                   else
071800160920     c                   z-add     999           §lppdr
071900160920     c                   end
072000160920      *
072100160629     c                   movel     §lplnp        §lppdr
072200160629      *
072300160629      ******  DA SOSTITUIRE PRENDENDOLO DALLA TABELLA
072400160630     c                   eval      §lpCTM= §nsdCTM
072500160629     C                   clear                   DTASV
072600160629     C                   clear                   trul90ds
072700160629      ******
072800160916     C                   CALL      'FNLS01R'                            99
072900160627     C                   PARM                    KPJBA
073000160627     C                   PARM                    DBLP
073100160627     C                   PARM                    DTASV
073200160627     C                   PARM                    trul90ds
073300160627      *
073400160629      *  Se non c'è riuscito a scrivere la bolla
073500160916     c                   if        §LPFPR='5' or *in99
073600160629     c                   move      'S'           scrivereVAB
073700160629     c                   end
073800160317      *
073900160317     c                   Endsr
074000160718      *---------------------------------------------------------------*
074100160718      *?  Reperimento Dati del job (Utente/Operativi)                ?
074200160718      *---------------------------------------------------------------*
074300160718     c     DatiJOB       BEGSR
074400160718      *
074500160718     c     *dtaara       define    §azute        azuteds
074600160718     c     *dtaara       define    §datiute      ddatiute
074700160718      *
074800160719     c     *lock         in(E)     *dtaara
074900160718     c                   IF        %ERROR or RSUT = *blanks
075000160718     c                   clear                   Tibs34Ds
075100160718     c                   call      'TIBS34R'
075200160718     c                   parm                    Tibs34Ds
075300160718     c                   in        *dtaara
075400160718     c                   ENDIF
075500160718      *
075600160718     c                   ENDSR
075700160718     C*---------------------------------------------------------------*
