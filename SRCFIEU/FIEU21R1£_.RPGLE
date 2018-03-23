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
002000160629     Ffibsp01l  iF   E           K DISK
002100991129     FTNTBE01L  IF   E           K DISK
002200050112     FTABEL00F  IF   E           K DISK
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
004800160317      *-------------------
004900160317     D KPJBA         E DS
005000160317     D OG143         E DS
005100160317     D DDPP          E DS
005200160317     D DLPD          E DS
005300160318     D DNSD          E DS
005400160317     D tisie2ds      E DS
005500160317     D DpdcFLO       E DS
005600160317     D FIEULPDDS     E DS
005700160718      *--------------------------
005800160718     d* Parametri x Controllo profilo utenti
005900160718     d TIBS34ds      e ds
006000160718     d AZUTEds       e ds                  extname(AZUTE00F)
006100160718     d DDatiUte      e ds
006200160718     D*------------------
006300160718     D* DS REPEIMENTO TERMINALs
006400160718     D*------------------
006500160718     D FNLV55DS      E DS
006600160718      *---------------------
006700160718      *--------------------------
006800991129     D W015A           S             15
006900091109     D W030A           S             30
007000000223     D W0140           S             14  0
007100991129     D WORA            S              6  0
007200991129     D XX              S              3  0 INZ
007300991129     D WDTGIO          S              8  0
007400991129     D DATEU           S              8  0
007500991129     D DATA_eur        S               D   DATFMT(*eur)
007600050112     D svkpjbu         s                   like(kpjbu)
007700060608      **?------------------------------------------------------------------ */
007800060612     D DVate         E DS
007900060620      *
008000160316     D DsBareCode      DS
008100160316     d  Percent                1      1
008200160316     d  CAPdest                2      8
008300160316     D  DepotParcel            9     12
008400160316     d  NrParcel               9     22
008500160316     d  ServCode              23     25
008600160316     d  Nazione               26     28
008700060608      **?------------------------------------------------------------------ */
008800050112      * Numeratore Bolle (302)
008900050112     D trul33ds      E DS
009000050112     D Ds3C          E DS
009100070202     D TISI95DS      E DS
009200160317     D KAAA            S                   LIKE(NUFAAA)
009300160317     D KCNU            S                   LIKE(NUFCNU)
009400160317     D KFIL            S                   LIKE(NUFFIL)
009500160317     D NUM_Sped        s                   LIKE(vabnsp)
009600060612      * ?   *--------------------------------------------------------------*
009700140827      **
009800060710     D Digits          C                   '0123456789'
009900160317     d Normal_Parcel   c                   '101'
010000160317     d Small_Parcel    c                   '136'
010100160317     d Return_Sender   c                   '298'
010200160317     d Return          c                   '300'
010300160317     d Gomme_365       c                   '365'
010400160317     d Gomme_366       c                   '366'
010500160317     d Gomme_367       c                   '367'
010600111110     d Normal_Parcel_327...
010700160317     d                 c                   '327'
010800111110     d Normal_Parcel_329...
010900160317     d                 c                   '329'
011000111110     d Small_Parcel_328...
011100160317     d                 c                   '328'
011200060612      * ?================================================================== */
011300160316     c                   SETON                                        LR
011400160316     c                   eval      DsBareCode = barecode28
011500160316      **
011600160316      **  Reso NON si scrive
011700160317     c                   if        ServCode  = Return_Sender
011800160317     c                               or
011900160317     c                             ServCode  = Return
012000160316     c                               or
012100160317     c                             DepotParcel = '0844'
012200160317     c                               or
012300160317     c                             DepotParcel = '0845'
012400160318      *
012500160318      * abilitato significa anche abilitato alla cancellazione del record
012600160318      *   poichè è riuscito a impostare il record bolla quindi può cancellare la spia
012700160318     c                   MOVE      'S'           abilitato         1
012800160317      * ?            ==============
012900160316     c                   return
013000160317      * ?            ==============
013100160316     c                   ENDIF
013200160317
013300160316      **?------------------------------------------------------------------ */
013400160316      * Clearizzo EDiVAB e FiVAB
013500160316     c                   clear                   edivab00
013600160316     c                   clear                   fivab000
013700160316     c                   clear                   su_EDIVAB         1
013800060612      **?------------------------------------------------------------------ */
013900160316      * ?Data  spedizione
014000160721     C                   MOVE      datcor        VABMGS                         = oggi
014100160317      * ?Tipo servizio/Colli
014200160316     c                   movel     'C'           vabtsp
014300160316     c                   z-add     1             vabncl
014400160721     C                   MOVEL     DATCOR        CAMPO4            4
014500160721     C                   MOVE      CAMPO4        VABAAS
014600160316     c
014700160316      * ?il Parcel deve essere impostato nella DS VATE
014800160316      * fisso il rif.numerico a (1) e sostituito se contiene solo numeri
014900160316     C                   z-add     1             vabrmn
015000160317     c     digits        check     NrParcel      position          3 0
015100160316      **  presenti campi non numerici: (Ossia trovati)
015200160606     c                   iF         %Found
015300160606     c                   MOVE      'S'           abilitato         1
015400160606     c                   RETURn
015500160606     c                   else
015600160606      * ?  per impostare il riferimento numerico (VABRMN):
015700160606     C                   move      NrParcel      vabrmn
015800160606     c                   enD
015900160317      * ?CAP
016000160318     c                   move      CAPdest       field5            5            C.A.P.
016100160318     c                   movel     field5        vabcad                         C.A.P.
016200160316      * ?Se manca la provincia la calcolo
016300160316     C                   CLEAR                   TISI95DS
016400160316     C                   MOVEL     vabCAD        I95CAP
016500160316     C                   MOVEL     DATEU         I95DAT
016600160316     C                   MOVEL     '3'           I95TCN
016700160316     C                   CALL      'TISI95R'
016800160316     C                   PARM                    TISI95DS
016900160316     C     O95ERR        IFeq      *BLANKS
017000160316     C                   MOVEL     O95PRV        vabPRD
017100160329     C                   Else
017200160329      * x errore NON scrive la spedizione
017300160329     C                   RETURN
017400160329     C                   END
017500160316      *
017600160316      ** imposta il Depot di provenienza x testare se VTG
017700160316     c                   clear                   parcel_VTG        1
017800160316     C                   movel(p)  'VTG'         cldCOD
017900160316     C                   move      DepotParcel   cldDEP
018000160316     c     key_VTG       setll     tncld00F
018100160316     c     key_VTG       reade     tncld00F
018200160316     c                   dow       not %EoF(tncld00F)
018300160316     c                   if        dateu >= cldDDA and
018400160316     c                             dateu <= cldADA
018500160316     c                   move      'S'           parcel_VTG
018600160316     c                   leave
018700160316     c                   end
018800160316     c     key_VTG       reade     tncld00F
018900160316     c                   endDO
019000160317      * ?Service  Code:
019100160316     c                   clear                   w015A
019200160317     c                   if        ServCode  = Normal_Parcel
019300160316     c                               or
019400160317     c                             ServCode  = Normal_Parcel_327
019500160317     c                               or
019600160317     c                             ServCode  = Normal_Parcel_329
019700160316     c                   eval      W015A = 'PESO01'
019800160316     c                   end
019900160317      **  traffico gomme.
020000160317     c                   if        ServCode  = Gomme_365
020100160316     c                               or
020200160317     c                             ServCode  = Gomme_366
020300160316     c                               or
020400160317     c                             ServCode  = Gomme_367
020500160316     c                   eval      W015A = 'PESO03'
020600160316     c                   end
020700160317      * piccolo
020800160317     c                   if        ServCode  = Small_Parcel
020900160316     c                               or
021000160317     c                             ServCode  = Small_Parcel_328
021100160316     c                   eval      W015A = 'PESO02'
021200160316     c                   end
021300160316      * se per qualche motivo non passano il codice pacco/peso imposto come
021400160316      *   pacco grande
021500160316     c                   if        W015A = *blank
021600160316     c                   eval      W015A = 'PESO01'
021700160316     c                   end
021800160316      *
021900160316      * ?Peso, ricavato dal tipo servizio del ROUTELABEL
022000160316     c                   z-add     1             xx
022100160316     c                   if        w015a <> *blank
022200160316     c     W015A         lookup    DPPK(xx)                               31
022300160316     c                   If        *in31 = *off
022400160316     c                   eval      W015A = 'PESO'
022500160316     c     W015A         lookup    DPPK(xx)                               31
022600160316     c                   Endif
022700160316     c                   z-add     DPP(xx)       vabPKB
022800160316     c                   z-add     vlb(xx)       vabVLB
022900160316     c                   End
023000160316      * ?Codice bolla
023100160316     C                   MOVEL     '1'           vabcbo                         NO C/assegno
023200160317      * ?Provenienza
023300160317     c                   movel     '1'           VABscl
023400160316      *
023500160316      * ?Controlla esistenza del Parcel sul file degli ORM dati a DPD per
023600160317      * ?assegnato ORM
023700160317     C     nrParcel      chain     fidpo06l
023800160316     c                   if        %Found(fidpo06l)
023900160316     C                   MOVEL     '2'           vabcbo
024000160316     c                   end
024100160316      *
024200160316      * ?SE si tratta di un ORM commissionato a DPD, Controllo se deve essere
024300160316      * ?in FERMO DEPOSITO.
024400160316     c                   eval      vabFFD = *blank
024500160316     C                   if         vabcbo = '2'  and dpoPOE > 0
024600160316     C                   z-add     dpoPOE        ORMPOE
024700160316     C                   z-add     dpoNRS        ORMNSR
024800160316     C                   z-add     dpoNOR        ORMNOR
024900160316     C                   z-add     dpoNRV        ORMNRV
025000160317      * ?sull'ORM se Fermo Deposito
025100160316     C     KORM          chain     fnORM01l
025200160316     c                   if        %Found(FnOrm01l) and ormffd ='S'
025300160316     c                   eval      vabFFD = 'S'
025400160316     c                   end
025500160316     c                   end
025600160317      **
025700160317      *--------
025800160317     C* con la Linea di chi spara prende quella DPD da sostituire su LNP e FGS
025900160317     c     Filiale       chain     azORG01l
026000160317     c                   if          %Found(azORG01l)
026100160317     c                   movel     orgde3        OG143
026200160317     c                   if           §OGLNPB <> *blank and §OGLNPB <> '000'
026300160318     c                   move      §OGLNPB       VABLNP
026400160318     c                   move      §OGLNPB       VABFGS
026500160318     c                   move      §OGLNPB       Linea_Partenza    3 0
026600160317     c                   end
026700160317     c                   end
026800160318      **
026900160318      **  ricava la LNP e altri valori fondamentali dal PARCEL/DEPOT di Partenza
027000160318     c                   exsr      LNP_da_dove
027100160318      *
027200160318      ** tab.definizione dati x Linea DPD
027300160318     c                   exsr      tabella_LPD
027400160317      *
027500160318      * da tabella NSD (1)
027600160318     c                   movel     §NSDRSD       VABRSD
027700160318     c                   movel     §NSDIND       VABIND
027800160318     c                   movel     §NSDLOD       VABLOD
027900160318      *
028000160317      * abilitato significa anche abilitato alla cancellazione del record
028100160317      *   poichè è riuscito a impostare il record bolla quindi può cancellare la spia
028200160317     c                   MOVE      'S'           abilitato         1
028300060612      * -----------------                  -------------------
028400160720      * ?scrive SPED.VEDI PACCO in PARTENZA BLP
028500160627     C                   EXSR      Scrive_BLP
028600160720      * -----------------                  -------------------
028700160627      *
028800151113      * ?==============================
028900060612      * ?Scrivo EDiVAB o FiVAB
029000151113      * ?==============================
029100160629     c                   if            scrivereVAB ='S'
029200160720      * -----------------                  -------------------
029300160720      * ?Numero spedizione
029400160720     C                   EXSR      SUB_keybol
029500160720      * -----------------                  -------------------
029600160317     C                   IF        su_EDIVAB = 'S'
029700160317     c                   eval      vabcmr = 'GEO_' + %editc(dateu:'Y') + '_PST'
029800060612     c                   z-add     DATEU         VABdcm
029900060612     c                   z-add     DATEU         VABdts
030000060612     c                   movel     WORA          VABhms
030100060612     c                   write     EDivab00
030200060612     c                   ELSE
030300060612     c                   write     FiVAB000
030400060612     c                   ENDIF
030500060614      ***
030600160316     c                   exsr      wrtvat
030700160720     c                   end
030800060612      *
030900160317     c                   Return
031000060612     C*----------------------------------------------------------------
031100060612     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
031200060612     C*----------------------------------------------------------------
031300060612     C     WRTVAT        BEGSR
031400060612      *
031500060413      * ? Parcel Nr. --> Riferimento Parcel DPD
031600051219     C                   If        su_EDIVAB = 'S'
031700041215     c                   clear                   edivat00
031800041215     c                   else
031900041215     c                   clear                   fivat000
032000041215     c                   end
032100160316      **  su nuovo tipo record (E)
032200160316     C                   MOVEL     'E'           VATTRC
032300041215     C                   movel     VABfgs        VATfgs
032400041215     C                   Z-ADD     VABCCM        VATCCM
032500041215     C                   Z-ADD     VABAAS        VATAAS
032600041215     C                   Z-ADD     VABLNP        VATLNP
032700041215     C                   Z-ADD     VABNRS        VATNRS
032800041215     C                   Z-ADD     VABNSP        VATNSP
032900060621      *
033000060621      *  IMPOSTA la DS VAT(E)
033100160316     c                   clear                   DVate
033200160316     C                   MOVEL     NrParcel      §VATEPRN
033300060620     C                   move      *zeros        §VATESCD
033400160316     C                   move      ServCode      §VATESCD
033500160316     C                   move(p)   '%'           §VATEBID
033600060608     C                   MOVEL     *blank        §VATEROU
033700060620      *
033800060608     C                   MOVEL     DVate         VATNOT
033900041215      **
034000051219     C                   If        su_EDIVAB = 'S'
034100041215     C                   MOVEL     VABCMR        VATCMR
034200041215     C                   Z-ADD     VABCNT        VATCNT
034300041215     C                   WRITE     EDiVAT00
034400041215     c                   else
034500041215     C                   WRITE     fiVAT000
034600041215     c                   end
034700060413      *
034800041215     C                   ENDSR
034900971215      *----------------------------------------------------------------
035000971216      * DETERMINO DATA E CHIAVE SPEDIZIONE
035100971215      *----------------------------------------------------------------
035200991206     C     SUB_keybol    BEGSR
035300050112      *
035400050112      **              **------------------**
035500050112      *    deve controllare sulla tabella "3C" se il numero spedizione
035600050112      *     deve essere mantenuto oppure no
035700050112     C                   clear                   Ds3C
035800060620     C                   Z-ADD     1             TBLKUT
035900050112     C                   MOVEL     '3C'          TBLCOD
036000050112     C                   movel(p)  VABccm        TBLKEY
036100160317     C     KTABel        CHAIN     TABEL00F
036200050112     C                   IF        %Found(TABEL00F) and tblflg = *blank
036300050112     C                   MOVEL     TBLUNI        Ds3C
036400050112     C                   END
036500050112      *
036600050112      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
036700050112     c                   if        §3CFsp <> 'S'
036800050112      *
036900050112     C* NSP => Stacco un numeratore da AZNUM
037000050112     c                   movel     kpjbu         svkpjbu
037100050112     c                   clear                   kpjbu
037200050112     C                   clear                   TRUL33DS
037300050112     C                   eval      I33OPE = *zeros
037400050112     C                   eval      I33CNU = 302
037500050112     C                   eval      I33NUM = 1
037600050112     C                   movel     TRUL33DS      KPJBU
037700050112     C                   call      'TRUL33R'
037800050112     C                   parm                    KPJBA
037900050112     C                   movel     KPJBU         TRUL33DS
038000050112     C                   if        O33ERR = *zeros
038100050112     c                   z-add     o33nrf        NUM_SPED
038200050112     C                   endif
038300050112     c                   movel     svkpjbu       kpjbu
038400050112      ****
038500050112     c                   ELSE
038600050112      *   se si vuole mantenere il numero spedizione
038700050112      ****
038800160721     C                   MOVEl     datCOR        KAAA
038900050112     C                   Z-ADD     3             KCNU
039000050112     C                   Z-ADD     VABLNP        KFIL
039100050112     C     KNUF          CHAIN     FLNUF                              9999
039200050112     C                   IF        *IN99 = *OFF
039300050112     C                   ADD       1             NUFNUM
039400050112     C                   Z-ADD     NUFNUM        NUM_Sped                       *NUM.SPEDIZIONE
039500050112     C                   UPDATE    FLNUF
039600060612     c                   END
039700060411      *
039800050112     c                   EndIF
039900050112      **
040000050112      **              **------------------**
040100050112     C                   Z-ADD     NUM_Sped      VABNSP                         *NUM.SPEDIZIONE
040200050112      **              **------------------**
040300160721     C                   MOVEL     DATCOR        CAMPO4            4
040400160721     C                   MOVE      CAMPO4        KAAA
040500160721     C                   MOVE      CAMPO4        VABAAS
040600971216      *
040700991206     C                   ENDSR
040800051205      *----------------------------------------------------*
040900051205      *   DEFINIZIONE CHIAVI                               *
041000051205      *----------------------------------------------------*
041100051205     C     *INZSR        BEGSR
041200051205      *
041300991129     c     *ENTRY        PLIST
041400160317     C                   parm                    Filiale           3 0
041500160316     C                   parm                    BareCode28       28
041600160316     C                   parm                    Abilitato         1
041700160719     C                   parm                    kpjba
041800160316      *
041900160718      *  prende i dati utente
042000160718     c                   exsr      DATIjob
042100160718      *
042200160316      *  se scriverà correttamente il VAB deve impostare "S" in abilitato
042300160316      *  che significa che il programma chiamante può cancellare la spunta
042400160316      *  poichè già utilizzata.
042500991129      *
042600991206     C     KNUF          KLIST
042700991206     C                   KFLD                    KAAA
042800991206     C                   KFLD                    KCNU
042900991206     C                   KFLD                    KFIL
043000160317      *
043100160317     C     key_Depot     KLIST
043200160317     C                   KFLD                    vers_CAP_DPD
043300160317     C                   KFLD                    DepotParcel
043400160317      *
043500160317     C     key_Dep       KLIST
043600160317     C                   KFLD                    DepotParcel
043700160317     C                   KFLD                    DATEU
043800971215      *
043900050112     C     KTABel        KLIST
044000050112     C                   KFLD                    TBLKUT
044100050112     C                   KFLD                    TBLCOD
044200050112     C                   KFLD                    TBLKEY
044300160318      *
044400160318     C     KTBE          KLIST
044500160318     C                   KFLD                    TBECOD
044600160318     C                   KFLD                    TBEKE1
044700100920      *
044800100920     C     KORM          KLIST
044900100920     C                   KFLD                    ORMPOE
045000100920     C                   KFLD                    ORMNSR
045100100920     C                   KFLD                    ORMNOR
045200100920     C                   KFLD                    ORMNRV
045300150604      *
045400150604     C     key_VTG       KLIST
045500150604     C                   KFLD                    cldCOD
045600150604     C                   KFLD                    cldDEP
045700160301      **
045800050414      *------------------
045900020919      * Apro file transito o FiVAB pilotato dalla tabella
046000991124      * Caricamento Tabella PESI DPD
046100060620     c                   clear                   XX
046200991124     c                   clear                   DPP
046300991124     c     'DPP'         chain     tntbe000                           31
046400991124      *
046500991124     c                   DOW       *in31 = *off
046600991124      * Se il S.I. è indicato deve essere uguale al mio
046700991124     c                   IF        TBEatb = *BLANKS  and
046800991129     c                              (TBEsif = Knsif  or  TBEsif = *blanks)
046900991124     c                   add       1             xx
047000991124     c                   movel     TBEke1        DPPK(XX)
047100000406     c                   movel     TBEuni        DDPP
047200000410     c                   movel     §DPPpkb       DPP(XX)
047300141210     c                   movel     §DPPvlb       vlb(XX)
047400991124     c                   endif
047500991124     c     'DPP'         reade     tntbe000                               31
047600991124     c                   enddo
047700160318      *
047800160318     c                   movel(p)  'NSD'         tbeCOD
047900160318     c                   movel(p)  '1'           tbeKE1
048000160318     C     KTBE          chain     tntbe01l
048100160318     c                   if        %Found(tntbe01l)  and TBEatb = *BLANKS
048200160318     c                   eval      dNSD = tbeUNI
048300160318     c                   end
048400160317      *
048500160317     c                   z-add     0             vers_CAP_DPD      5 0
048600160317     c* recupero l'ultima versione di cappario DPD
048700160317     c                   clear                   tisie2ds
048800160317     c                   eval      sie2dri=*all'9'
048900160317     c                   call      'TISIE2R'
049000160317     c                   parm                    tisie2ds
049100160317     c* se trovata versione reperisco  descrizioni deposito
049200160317    2c                   if        sie2ver > 0
049300160317     c                   z-add     sie2ver       vers_CAP_DPD
049400160317     c                   end
049500160317      *
049600160317      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
049700160317     c                   movel     kpjbu         svkpjbu
049800160317     C                   CLEAR                   TRUL06DS
049900160317     C                   MOVE      '£1'          D06COD
050000160317     C                   MOVEL     Filiale       D06KEY
050100160317     C                   MOVEL     'L'           D06TLA
050200160317     C                   MOVEL     TRUL06DS      KPJBU
050300160317     C                   CALL      'TRUL06R'
050400160317     C                   PARM                    KPJBA
050500160317     c                   MOVEL     KPJBU         TRUL06DS
050600160317     c                   eval      kpjbu = svkpjbu
050700000908      *
050800971216      * Recupero data e ora
050900971216     C                   TIME                    WORA
051000991124     C                   TIME                    W0140
051100991124      * UDATE IN GGMMAAAA
051200991124     C                   MOVE      W0140         WDTGIO
051300991124      * UDATE IN AAAAMMGG
051400991124     C     *eur          MOVEL     WDTGIO        DATA_eur
051500991124     C     *iso          MOVEL     DATA_eur      dateu
051600160718     C                   MOVEL     dateu         datcor            8 0
051700141103      *-------------
051800991124     C                   ENDSR
051900060621      * ?------------------------------------------------------------------ */
052000060621      *?      X non bloccare in nessun caso il traduttore CLIENTI
052100060621      * ?------------------------------------------------------------------ */
052200060621     C     *pssr         BEGSR
052300060621     C
052400120504     C*
052500160318     C                   eval      wrkMsg='Creazione Bolla DPD da Spunta VDL h-
052600160318     C                             a avuto problemi e non ha generato la spedi-
052700160318     C                             zione dal FIBARDPD0F con il barecode :'
052800160318     C                             + barecode28 + '.'
052900090605      **    avviso tramite MAIL
053000090605     C                   exSR      eMail_alert
053100090605      **
053200060621     C                   ENDSR     '*CANCL'
053300060621     C
053400090605      * ?------------------------------------------------------------------ */
053500160316      *?  Invio Avviso x problemi nella creazione da spunta DPD
053600090605      * ?------------------------------------------------------------------ */
053700090605     C     eMail_alert   BEGSR
053800090605     C*
053900090605     C                   eval      wrkEml =
054000090605     C                             'Andrea.Bertocchi@bartolini.it'
054100140828     C*
054200160316     C                   eval      wrkMsg = 'Problemi durante la scrittura del -
054300160316     C                             VAB mentre leggeva il FIBARDPD con parcel :'
054400140828     c                             +':/N'+
054500160316     C                             %subst(BARECODE28:9:14)
054600090605     C*
054700160316     C                   eval      wrkOgg='Scrittura VAB da spunta VDL con -
054800160316     C                             (FIBARDPD0F)'
054900090605     C*
055000140828     C                   call(e)   'TRTCT00R2'
055100090605     C                   parm                    wrkEml          253
055200090605     C                   parm                    wrkOgg           44
055300090605     C                   parm                    wrkMsg         5000
055400090605      *
055500090605     C                   eval      wrkEml =
055600090605     C                             'CedAlert@bartolini.it'
055700090605     C*
055800140828     C                   call(e)   'TRTCT00R2'
055900090605     C                   parm                    wrkEml          253
056000090605     C                   parm                    wrkOgg           44
056100090605     C                   parm                    wrkMsg         5000
056200090605      *
056300090605     C                   ENDSR
056400060614      *-------------------------------------------------------------------------
056500160317      *  in base al Depot di provenienza determina Linea e Cliente
056600160317      *----------------------------------------------------------------
056700160317     C     LNP_da_dove   BEGSR
056800160318      **
056900160318      * Default
057000160318      ** aggancia i dati della tab.LPD  di default
057100160318     c                   if          Linea_Partenza = 0
057200160318     c                   z-add     190           Linea_Partenza    3 0
057300160318     c                   end
057400160317      *
057500160317      *  con il DEPOT di provenienza  aggancia x Versione il Cappario DPD
057600160317      *   e se non fosse presente con i primi 4 caratteri del Parcel
057700160317      *
057800160317     C     key_Depot     chain     DPcdp01l
057900160317     c                   if        %Found(DPcdp01l)
058000160317      *
058100160317     C     key_Dep       setgt     DPpdc01l
058200160317     C     DepotParcel   readpe    DPpdc01l
058300160317     c                   dow       not %Eof(DPpdc01l)
058400160317      *   se trovato il Depot VALIDO
058500160317     c                   if        pdcDDE <= dateU and dateU <= pdcDSC
058600160317     c                   eval      DpdcFLO = pdcFLO
058700160317      *
058800160317     c                   if        §PDCFLOLNP <> *blank  and
058900160318     c                             §PDCFLOLNP <> '000'   and
059000160318     c                             %check(digits:§PDCFLOLNP)=0
059100160318     c                   move(p)   §PDCFLOLNP    linea_PARCEL      3 0
059200160318      **
059300160318      **  controlla se appartiene alle linee della £1
059400160318     c     linea_PARCEL  lookup    LIN                                    25
059500160318     c                   if        %Found
059600160317     c                   move      §PDCFLOLNP    linea_partenza    3 0
059700160318     c                   leave
059800160318     c                   end
059900160318      **
060000160317     c                   end
060100160317     c                   end
060200160317      *
060300160317     C     DepotParcel   readpe    DPpdc01l
060400160317     c                   Enddo
060500160317     c                   End
060600160318      *
060700160317     c                   Endsr
060800160317     C*----------------------------------------------------------------
060900160317     C*  Aggancia tab.LPD ed imposta i campi del VAB
061000160317     C*----------------------------------------------------------------
061100160317     C     tabella_LPD   BEGSR
061200160317      *
061300160317     c                   clear                   FIEULPDds
061400160318      *
061500160317     c                   movel(p)  linea_partenzaiLPDlnp
061600160318     c                   movel(p)  linea_partenzaiLPDlna
061700160317     c                   movel(p)  nrParcel      iLPDparcel
061800160318      *
061900160317     c                   call      'FIEULPDR'
062000160317     c                   parm                    FIEULPDds
062100160317      *
062200160317      * ha trovato i dati necessari per instradare il record della spedizione
062300160317      * alla filiale di partenza
062400160318     c                   clear                   dLPD
062500160318     c                   if         oLPDtip <> 'E'
062600160318     c                   eval           dLPD = oLPDUNI
062700160317     C                   eval      su_EDIVAB = §LPDWRK
062800160318     C                   eval         vabCCM = §LPDKSC
062900160318     C                   eval         vabCTM = §LPDCTM
063000160330      *
063100160330      ** tariffa non sarebbe corretta
063200160330     C                   eval         vabCTR = OLPDTAR
063300160318     C                   movel     linea_partenzaVABLNP
063400160318     C                   movel     linea_partenzaVABFGS
063500160317     c                   end
063600160317      *
063700160317      *  QUI la tariffa LHS non deve essere considerata poichè posso conoscere
063800160317      *  la linea di PARTENZA ma la linea di ARRIVO non la conosco.
063900160627      *-------------
064000160627     C                   ENDSR
064100160627      * ?------------------------------------------------------------------ */
064200160627      *?      Scrive il BLP - bolla automatica come VEDI PACCO
064300160627      * ?------------------------------------------------------------------ */
064400160627     C     Scrive_BLP    BEGSR
064500160718     C*
064600160718     C* REPERISCE IL TERMINAL PARTENZA x l'area dati 34DS
064700160718     C                   CLEAR                   FNLV55DS
064800160718     C                   MOVEL     'P'           D55TPT
064900160718     C                   MOVEL     vablnp        D55LIN
065000160718     C                   Z-ADD     DATCOR        D55DRF
065100160718     C                   CALL      'FNLV55R'
065200160718     C                   PARM                    FNLV55DS
065300160718IF  1C     D55ERR        IFNE      *BLANKS
065400160718     C                   MOVEL     vablnp        D55TFP
065500160718E   1C                   ENDIF
065600160718     C*
065700160719      ** imposta il SIMFEL come terminal di partenza
065800160719      *   Aggiorno area dati Dati Utente
065900160719     C     *lock         in        DDatiUte
066000160718     c                   movel     d55tfp        simfel
066100160719     C                   out       DDatiUte
066200160719     C                   UnLock(e) DDatiUte
066300160718      *
066400160629     c                   move      'N'           scrivereVAB       1
066500160629      *
066600160627      * altrimentideve se non c'è riuscito, scrive il VAB ed il VAT
066700160627     c                   clear                   dblp
066800160629     C                   MOVEL     vabmgs        §lpmgs                         = oggi
066900160629     c                   movel     vabtsp        §lptsp
067000160629     c                   z-add     vabncl        §lpncl
067100160629     C                   z-add     vabrmn        §lprmn
067200160629     c                   eval      §lpnpr=Nrparcel
067300160629     c                   Movel     vabcad        §LpCad
067400160629     C                   MOVEL     vabprd        §lpprd
067500160629     c                   z-add     vabpkb        §lpPKB
067600160629     c                   z-add     vabvlb        §lpVLB
067700160629     C                   MOVEL     vabcbo        §lpcbo                         NO C/assegno
067800160627      * ?Provenienza
067900160627     c                   move      *all'9'       §lppoe
068000160627     c                   move      *all'9'       §lpnsr
068100160627     c                   move      *all'9'       §lpnor
068200160627     c                   move      *all'9'       §lpnrv
068300160629      * ?SE aveva un ORM legato.
068400160627     C                   if         §lpcbo = '2'  and dpoPOE > 0
068500160629     C                   z-add     dpoPOE        §LPPOE
068600160629     C                   z-add     dpoNRS        §LPNSR
068700160629     C                   z-add     dpoNOR        §LPNOR
068800160629     C                   z-add     dpoNRV        §LPNRV
068900160629     c                   eval      §lpffd = vabffd
069000160629     c                   end
069100160630     c                   z-add     vabaas        §lpaas
069200160718     c                   z-add     vablnp        §lplnp
069300160629     c                   movel     vabfgs        §lpfgs
069400160629     c                   movel     vabRSD        §lpRSD
069500160629     c                   movel     vabIND        §lpIND
069600160629     c                   movel     vabLOD        §lpLOD
069700160630     c                   z-add     vabccm        §lpccm
069800160630     c                   z-add     vabccm        §lpccn
069900160627      *
070000160627     c* per il momento imposto ccn = ccm
070100160627     c                   eval      §lpdrt=dateu
070200160627     c                   eval      §LPFPP='P'
070300160629     c                   eval      §lpfpr='D'
070400160629     c                   eval      §lpsop=':'
070500160627     c                   clear                   dblp01
070600160627     c                   eval      §LPscd=ServCode
070700160627     c                   movel     '1'           §lptrd
070800160627     c                   movel     dblp01        §lpflo
070900160629      **  L'Autista
071000160629     c* il padroncino è da prendere da tb bsp: altrimenti imposto fisso 999
071100160629     c     §lplnp        chain     fibsp01l
071200160629     c                   if        %found(fibsp01l)
071300160629     c                   z-add     bspPDR        §lppdr
071400160629     c                   else
071500160627     c                   z-add     999           §lppdr
071600160629     c                   end
071700160629     c                   movel     §lplnp        §lppdr
071800160629      *
071900160629      ******  DA SOSTITUIRE PRENDENDOLO DALLA TABELLA
072000160630     c                   eval      §lpCTM= §nsdCTM
072100160629     C                   clear                   DTASV
072200160629     C                   clear                   trul90ds
072300160629      ******
072400160627     C                   CALL      'FNLS01R'
072500160627     C                   PARM                    KPJBA
072600160627     C                   PARM                    DBLP
072700160627     C                   PARM                    DTASV
072800160627     C                   PARM                    trul90ds
072900160627      *
073000160629      *  Se non c'è riuscito a scrivere la bolla
073100160629     c                   if        §LPFPR='5'
073200160629     c                   move      'S'           scrivereVAB
073300160629     c                   end
073400160317      *
073500160317     c                   Endsr
073600160718      *---------------------------------------------------------------*
073700160718      *?  Reperimento Dati del job (Utente/Operativi)                ?
073800160718      *---------------------------------------------------------------*
073900160718     c     DatiJOB       BEGSR
074000160718      *
074100160718     c     *dtaara       define    §azute        azuteds
074200160718     c     *dtaara       define    §datiute      ddatiute
074300160718      *
074400160719     c     *lock         in(E)     *dtaara
074500160718     c                   IF        %ERROR or RSUT = *blanks
074600160718     c                   clear                   Tibs34Ds
074700160718     c                   call      'TIBS34R'
074800160718     c                   parm                    Tibs34Ds
074900160718     c                   in        *dtaara
075000160718     c                   ENDIF
075100160718      *
075200160718     c                   ENDSR
075300160718     C*---------------------------------------------------------------*
