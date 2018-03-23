000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000301160616      ***
000302160616      ** Prova creazione Vedi Pacco DPD in batch
000303160616      ***
000400060609      **?************************************************************************
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
002200130618     FazORG01L  IF   E           K DISK
002300971216      *
002400150604     FTNCLD00F  IF   E           K DISK
002500061016     Ffidpo06l  IF   E           K DISK
002600160318     Fdpcdp01l  IF   E           K DISK
002700160318     Fdppdc01l  IF   E           K DISK
002800061016      *
002900050414     FFiVAB01L  UF A E           K DISK
003000050414     FEDivab3L  UF A E           K DISK
003100041215      *
003200050414     FEDiVAT3L  Uf A E           K DISK
003300050414     FFiVAT01L  Uf A E           K DISK
003400160317      *----------------------------------------------------*
003500160317     D TRUL06DS      E DS
003600160317     D  LIN                    1     90  0 DIM(30)
003700940321      *----------------------------------------------------*
003800991124      * Schiere per conversione peso DPD
003900991124     D DPPK            S             15    DIM(30)                              CODICE NAZIONE
004000991124     D DPP             S              7S 1 DIM(30)                              CODICE NAZIONE
004100141210     D VLB             S              5S 3 DIM(30)                              CODICE NAZIONE
004200160317      *-------------------
004300160317     D KPJBA         E DS
004301160616     d dblp          e ds
004302160616     d dblp01        e ds
004303160616     d dtasv         e ds                  inz
004304160616     d trul90ds      e ds                  inz
004400160317     D OG143         E DS
004500160317     D DDPP          E DS
004600160317     D DLPD          E DS
004700160318     D DNSD          E DS
004800160317     D tisie2ds      E DS
004900160317     D DpdcFLO       E DS
005000160317     D FIEULPDDS     E DS
005100000907      *-------------------
005200991129     D W015A           S             15
005300091109     D W030A           S             30
005400000223     D W0140           S             14  0
005500991129     D WORA            S              6  0
005600991129     D XX              S              3  0 INZ
005700991129     D WDTGIO          S              8  0
005800991129     D DATEU           S              8  0
005900991129     D DATA_eur        S               D   DATFMT(*eur)
006000050112     D svkpjbu         s                   like(kpjbu)
006100060608      **?------------------------------------------------------------------ */
006200060612     D DVate         E DS
006300060620      *
006400160316     D DsBareCode      DS
006500160316     d  Percent                1      1
006600160316     d  CAPdest                2      8
006700160316     D  DepotParcel            9     12
006800160316     d  NrParcel               9     22
006900160316     d  ServCode              23     25
007000160316     d  Nazione               26     28
007100060608      **?------------------------------------------------------------------ */
007200050112      * Numeratore Bolle (302)
007300050112     D trul33ds      E DS
007400050112     D Ds3C          E DS
007500070202     D TISI95DS      E DS
007600160317     D KAAA            S                   LIKE(NUFAAA)
007700160317     D KCNU            S                   LIKE(NUFCNU)
007800160317     D KFIL            S                   LIKE(NUFFIL)
007900160317     D NUM_Sped        s                   LIKE(vabnsp)
007901160616     d wtxt            S           2048
007902160616     d TxtInOut        S           2048
007903160616     d ElencoChar      S            256
007904160616     d TipoElenco      S              1
007905160616     d CharSost        S              1
007906160616     d UpperCase       S              1
007907160616     d ChkNull         S              1    Inz('1')
007908160616     d CharNull        S              1
007909160616     d Esito           S              1
008000060612      * ?   *--------------------------------------------------------------*
008100140827      **
008200060710     D Digits          C                   '0123456789'
008300160317     d Normal_Parcel   c                   '101'
008400160317     d Small_Parcel    c                   '136'
008500160317     d Return_Sender   c                   '298'
008600160317     d Return          c                   '300'
008700160317     d Gomme_365       c                   '365'
008800160317     d Gomme_366       c                   '366'
008900160317     d Gomme_367       c                   '367'
009000111110     d Normal_Parcel_327...
009100160317     d                 c                   '327'
009200111110     d Normal_Parcel_329...
009300160317     d                 c                   '329'
009400111110     d Small_Parcel_328...
009500160317     d                 c                   '328'
009600060612      * ?================================================================== */
009700160316     c                   SETON                                        LR
009800160316     c                   eval      DsBareCode = barecode28
009900160316      **
010000160316      **  Reso NON si scrive
010100160317     c                   if        ServCode  = Return_Sender
010200160317     c                               or
010300160317     c                             ServCode  = Return
010400160316     c                               or
010500160317     c                             DepotParcel = '0844'
010600160317     c                               or
010700160317     c                             DepotParcel = '0845'
010800160318      *
010900160318      * abilitato significa anche abilitato alla cancellazione del record
011000160318      *   poichè è riuscito a impostare il record bolla quindi può cancellare la spia
011100160318     c                   MOVE      'S'           abilitato         1
011200160317      * ?            ==============
011300160316     c                   return
011400160317      * ?            ==============
011500160316     c                   ENDIF
011600160317
012200060612      **?------------------------------------------------------------------ */
012201160616     c                   clear                   dblp
012300160316      * ?Data  spedizione
012400160616     C                   MOVEL     UMONTH        §lpmgs                         = oggi
012500160616     C                   MOVE      Uday          §lpmgs                         = oggi
012600160317      * ?Tipo servizio/Colli
012700160616     c                   movel     'C'           §lptsp
012800160616     c                   z-add     1             §lpncl
012900160316     c
013000160316      * ?il Parcel deve essere impostato nella DS VATE
013100160316      * fisso il rif.numerico a (1) e sostituito se contiene solo numeri
013200160616     C                   z-add     1             §lprmn
013300160317     c     digits        check     NrParcel      position          3 0
013400160316      **  presenti campi non numerici: (Ossia trovati)
013500160606     c                   iF         %Found
013600160606     c                   MOVE      'S'           abilitato         1
013700160606     c                   RETURn
013800160606     c                   else
013900160606      * ?  per impostare il riferimento numerico (VABRMN):
014000160616     C                   move      NrParcel      §lprmn
014100160606     c                   enD
014101160616     c                   eval      §lpnpr=nrparcel
014200160317      * ?CAP
014300160318     c                   move      CAPdest       field5            5            C.A.P.
014401160616     c                   Eval      wtxt = field5
014402160616     c                   ExSr      Sr_Chk
014403160616     c                   Movel     wtxt          §LpCad
014500160316      * ?Se manca la provincia la calcolo
014600160316     C                   CLEAR                   TISI95DS
014700160616     C                   MOVEL     §lpcad        I95CAP
014800160316     C                   MOVEL     DATEU         I95DAT
014900160316     C                   MOVEL     '3'           I95TCN
015000160316     C                   CALL      'TISI95R'
015100160316     C                   PARM                    TISI95DS
015200160316     C     O95ERR        IFeq      *BLANKS
015300160616     C                   MOVEL     O95PRV        §lpprd
015400160329     C                   Else
015500160329      * x errore NON scrive la spedizione
015600160329     C                   RETURN
015700160329     C                   END
015800160316      *
015900160316      ** imposta il Depot di provenienza x testare se VTG
016000160616     c********           clear                   parcel_VTG        1
016100160616     C*********          movel(p)  'VTG'         cldCOD
016200160616     C********           move      DepotParcel   cldDEP
016300160616     c**   key_VTG       setll     tncld00F
016400160616     c*    key_VTG       reade     tncld00F
016500160616     c*                  dow       not %EoF(tncld00F)
016600160616     c*                  if        dateu >= cldDDA and
016700160616     c*                            dateu <= cldADA
016800160616     c*                  move      'S'           parcel_VTG
016900160616     c*                  leave
017000160616     c*                  end
017100160616     c*    key_VTG       reade     tncld00F
017200160616     c*                  endDO
017300160317      * ?Service  Code:
017400160316     c                   clear                   w015A
017500160317     c                   if        ServCode  = Normal_Parcel
017600160316     c                               or
017700160317     c                             ServCode  = Normal_Parcel_327
017800160317     c                               or
017900160317     c                             ServCode  = Normal_Parcel_329
018000160316     c                   eval      W015A = 'PESO01'
018100160316     c                   end
018200160317      **  traffico gomme.
018300160317     c                   if        ServCode  = Gomme_365
018400160316     c                               or
018500160317     c                             ServCode  = Gomme_366
018600160316     c                               or
018700160317     c                             ServCode  = Gomme_367
018800160316     c                   eval      W015A = 'PESO03'
018900160316     c                   end
019000160317      * piccolo
019100160317     c                   if        ServCode  = Small_Parcel
019200160316     c                               or
019300160317     c                             ServCode  = Small_Parcel_328
019400160316     c                   eval      W015A = 'PESO02'
019500160316     c                   end
019600160316      * se per qualche motivo non passano il codice pacco/peso imposto come
019700160316      *   pacco grande
019800160316     c                   if        W015A = *blank
019900160316     c                   eval      W015A = 'PESO01'
020000160316     c                   end
020100160316      *
020200160316      * ?Peso, ricavato dal tipo servizio del ROUTELABEL
020300160316     c                   z-add     1             xx
020400160316     c                   if        w015a <> *blank
020500160316     c     W015A         lookup    DPPK(xx)                               31
020600160316     c                   If        *in31 = *off
020700160316     c                   eval      W015A = 'PESO'
020800160316     c     W015A         lookup    DPPK(xx)                               31
020900160316     c                   Endif
021000160616     c                   z-add     DPP(xx)       §lpPKB
021100160616     c                   z-add     vlb(xx)       §lpVLB
021200160316     c                   End
021300160316      * ?Codice bolla
021400160616     C                   MOVEL     '1'           §lpcbo                         NO C/assegno
021500160317      * ?Provenienza
021600160616     c******             movel     '1'           vabscl
021601160616     c                   move      *all'9'       §lppoe
021602160616     c                   move      *all'9'       §lpnsr
021603160616     c                   move      *all'9'       §lpnor
021604160616     c                   move      *all'9'       §lpnrv
021700160316      *
021800160316      * ?Controlla esistenza del Parcel sul file degli ORM dati a DPD per
021900160317      * ?assegnato ORM
022000160317     C     nrParcel      chain     fidpo06l
022100160316     c                   if        %Found(fidpo06l)
022200160616     C                   MOVEL     '2'           §lpcbo
022300160316     c                   end
022400160316      *
022500160316      * ?SE si tratta di un ORM commissionato a DPD, Controllo se deve essere
022600160316      * ?in FERMO DEPOSITO.
022700160616     c                   eval      §lpffd = *blank
022800160616     C                   if         §lpcbo = '2'  and dpoPOE > 0
022900160316     C                   z-add     dpoPOE        ORMPOE
023000160316     C                   z-add     dpoNRS        ORMNSR
023100160316     C                   z-add     dpoNOR        ORMNOR
023200160316     C                   z-add     dpoNRV        ORMNRV
023300160317      * ?sull'ORM se Fermo Deposito
023400160316     C     KORM          chain     fnORM01l
023500160316     c                   if        %Found(FnOrm01l) and ormffd ='S'
023600160616     c                   eval      §lpffd = 'S'
023601160616     c                   eval      §LPPOE = ormpoe
023602160616     c                   eval      §LPNSR = ORMNSR
023603160616     c                   eval      §LPNOR = ORMNOR
023604160616     c                   eval      §lpnrv = ORMNRV
023605160616
023700160316     c                   end
023800160316     c                   end
023900160317      **
024000160317      *--------
024100160317     C* con la Linea di chi spara prende quella DPD da sostituire su LNP e FGS
024200160317     c     Filiale       chain     azORG01l
024300160317     c                   if          %Found(azORG01l)
024400160317     c                   movel     orgde3        OG143
024500160317     c                   if           §OGLNPB <> *blank and §OGLNPB <> '000'
024600160616     c                   move      §OGLNPB       §lplnp
024700160616     c                   move      §OGLNPB       §lpfgs
024800160318     c                   move      §OGLNPB       Linea_Partenza    3 0
024900160317     c                   end
025000160317     c                   end
025100160318      **
025200160318      **  ricava la LNP e altri valori fondamentali dal PARCEL/DEPOT di Partenza
025300160318     c                   exsr      LNP_da_dove
025400160318      *
025500160318      ** tab.definizione dati x Linea DPD
025600160318     c                   exsr      tabella_LPD
025700160317      *
025800160318      * da tabella NSD (1)
025900160616     c                   movel     §NSDRSD       §lpRSD
026000160616     c                   movel     §NSDIND       §lpIND
026100160616     c                   movel     §NSDLOD       §lpLOD
026200160318      *
026300160318      *
026400160317      * abilitato significa anche abilitato alla cancellazione del record
026500160317      *   poichè è riuscito a impostare il record bolla quindi può cancellare la spia
026600160317     c                   MOVE      'S'           abilitato         1
026700060612      * -----------------                  -------------------
026800060612      * ?Numero spedizione
026900060612     C                   EXSR      SUB_keybol
027000151113      * ?==============================
027100060612      * ?Scrivo EDiVAB o FiVAB
027200151113      * ?==============================
027300160616     C***                IF        su_EDIVAB = 'S'
027400160616     c****               eval      vabcmr = 'GEO_' + %editc(dateu:'Y') + '_PST'
027500160616     c****               z-add     DATEU         VABdcm
027600160616     c****               z-add     DATEU         VABdts
027700160616     c***                movel     WORA          VABhms
027800160616     c*****              write     EDivab00
027900160616     c****               ELSE
028000160616     c*****              write     FiVAB000
028100160616     c***+               ENDIF
028200060614      ***
028300160616     c***                exsr      wrtvat
028301160616     c* per il momento imposto ccn = ccm
028302160616     c                   eval      §lpccn=§lpccm
028303160616     c                   eval      §lpdrt=dateu
028304160616     c                   eval      §LPFPP='P'
028305160616     c                   clear                   dblp01
028306160616     c                   eval      §LPscd=ServCode
028307160616     c                   movel     '1'           §lptrd
028308160616     c                   movel     dblp01        §lpflo
028309160616     c* il padroncino è da prendere da tb bsp: per il momento imposto fisso
028310160617     c****               eval      §lppdr=§lplnp+0999
028311160617     c                   z-add     999           §lppdr
028312160617     c                   movel     §lplnp        §lppdr
028313160616     c                   eval      §lpfpr='D'
028314160617     c                   eval      §lpsop=':'
028315160616
028316160616     C                   CALL      'FNLS01R'
028317160616     C                   PARM                    KPJBA
028318160616     C                   PARM                    DBLP
028319160616     C                   PARM                    DTASV
028320160616     C                   PARM                    trul90ds
028400060612      *
028500160317     c                   Return
028600060612     C*----------------------------------------------------------------
028700060612     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
028800060612     C*----------------------------------------------------------------
028900060612     C     WRTVAT        BEGSR
029000060612      *
029100060413      * ? Parcel Nr. --> Riferimento Parcel DPD
029200160616     C**                 If        su_EDIVAB = 'S'
029300160616     c**                 clear                   edivat00
029400160616     c**                 else
029500160616     c**                 clear                   fivat000
029600160616     c**                 end
029700160316      **  su nuovo tipo record (E)
029800160316     C                   MOVEL     'E'           VATTRC
029900041215     C                   movel     VABfgs        VATfgs
030000041215     C                   Z-ADD     VABCCM        VATCCM
030100041215     C                   Z-ADD     VABAAS        VATAAS
030200041215     C                   Z-ADD     VABLNP        VATLNP
030300041215     C                   Z-ADD     VABNRS        VATNRS
030400041215     C                   Z-ADD     VABNSP        VATNSP
030500060621      *
030600060621      *  IMPOSTA la DS VAT(E)
030700160316     c                   clear                   DVate
030800160316     C                   MOVEL     NrParcel      §VATEPRN
030900060620     C                   move      *zeros        §VATESCD
031000160316     C                   move      ServCode      §VATESCD
031100160316     C                   move(p)   '%'           §VATEBID
031200060608     C                   MOVEL     *blank        §VATEROU
031300060620      *
031400060608     C                   MOVEL     DVate         VATNOT
031500041215      **
031600160616     C***+               If        su_EDIVAB = 'S'
031700160616     C                   MOVEL     VABCMR        VATCMR
031800041215     C                   Z-ADD     VABCNT        VATCNT
031900041215     C                   WRITE     EDiVAT00
032000160616     c***                else
032100160616     C                   WRITE     fiVAT000
032200160616     c****               end
032300060413      *
032400041215     C                   ENDSR
032500971215      *----------------------------------------------------------------
032600971216      * DETERMINO DATA E CHIAVE SPEDIZIONE
032700971215      *----------------------------------------------------------------
032800991206     C     SUB_keybol    BEGSR
032900050112      *
033000050112      **              **------------------**
033100050112      *    deve controllare sulla tabella "3C" se il numero spedizione
033200050112      *     deve essere mantenuto oppure no
033300050112     C                   clear                   Ds3C
033400060620     C                   Z-ADD     1             TBLKUT
033500050112     C                   MOVEL     '3C'          TBLCOD
033600160616     C                   movel(p)  §lpccm        TBLKEY
033700160317     C     KTABel        CHAIN     TABEL00F
033800050112     C                   IF        %Found(TABEL00F) and tblflg = *blank
033900050112     C                   MOVEL     TBLUNI        Ds3C
034000050112     C                   END
034100050112      *
034200050112      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
034300050112     c                   if        §3CFsp <> 'S'
034400050112      *
034500050112     C* NSP => Stacco un numeratore da AZNUM
034600050112     c                   movel     kpjbu         svkpjbu
034700050112     c                   clear                   kpjbu
034800050112     C                   clear                   TRUL33DS
034900050112     C                   eval      I33OPE = *zeros
035000050112     C                   eval      I33CNU = 302
035100050112     C                   eval      I33NUM = 1
035200050112     C                   movel     TRUL33DS      KPJBU
035300050112     C                   call      'TRUL33R'
035400050112     C                   parm                    KPJBA
035500050112     C                   movel     KPJBU         TRUL33DS
035600050112     C                   if        O33ERR = *zeros
035700050112     c                   z-add     o33nrf        NUM_SPED
035800050112     C                   endif
035900050112     c                   movel     svkpjbu       kpjbu
036000050112      ****
036100050112     c                   ELSE
036200050112      *   se si vuole mantenere il numero spedizione
036300050112      ****
036400050112     C                   MOVE      UYEAR         KAAA
036500050112     C                   Z-ADD     3             KCNU
036600160616     C                   Z-ADD     §lpLNP        KFIL
036700050112     C     KNUF          CHAIN     FLNUF                              9999
036800050112     C                   IF        *IN99 = *OFF
036900050112     C                   ADD       1             NUFNUM
037000050112     C                   Z-ADD     NUFNUM        NUM_Sped                       *NUM.SPEDIZIONE
037100050112     C                   UPDATE    FLNUF
037200060612     c                   END
037300060411      *
037400050112     c                   EndIF
037500050112      **
037600050112      **              **------------------**
037700160616     C                   Z-ADD     NUM_Sped      §lpnsp                         *NUM.SPEDIZIONE
037800050112      **              **------------------**
037900050112     C                   MOVE      UYEAR         KAAA
038000160616     C     2000          add       kaaa          §lpAAS
038100971216      *
038200991206     C                   ENDSR
038300051205      *----------------------------------------------------*
038400051205      *   DEFINIZIONE CHIAVI                               *
038500051205      *----------------------------------------------------*
038600051205     C     *INZSR        BEGSR
038700051205      *
038701160616     c     *ENTRY        PLIST
038702160616     C                   parm                    Filiale           3 0
038703160616     C                   parm                    BareCode28       28
039200160316      *
039300160316      *  se scriverà correttamente il VAB deve impostare "S" in abilitato
039400160316      *  che significa che il programma chiamante può cancellare la spunta
039500160316      *  poichè già utilizzata.
039600991129      *
039700991206     C     KNUF          KLIST
039800991206     C                   KFLD                    KAAA
039900991206     C                   KFLD                    KCNU
040000991206     C                   KFLD                    KFIL
040100160317      *
040200160317     C     key_Depot     KLIST
040300160317     C                   KFLD                    vers_CAP_DPD
040400160317     C                   KFLD                    DepotParcel
040500160317      *
040600160317     C     key_Dep       KLIST
040700160317     C                   KFLD                    DepotParcel
040800160317     C                   KFLD                    DATEU
040900971215      *
041000050112     C     KTABel        KLIST
041100050112     C                   KFLD                    TBLKUT
041200050112     C                   KFLD                    TBLCOD
041300050112     C                   KFLD                    TBLKEY
041400160318      *
041500160318     C     KTBE          KLIST
041600160318     C                   KFLD                    TBECOD
041700160318     C                   KFLD                    TBEKE1
041800100920      *
041900100920     C     KORM          KLIST
042000100920     C                   KFLD                    ORMPOE
042100100920     C                   KFLD                    ORMNSR
042200100920     C                   KFLD                    ORMNOR
042300100920     C                   KFLD                    ORMNRV
042400150604      *
042500150604     C     key_VTG       KLIST
042600150604     C                   KFLD                    cldCOD
042700150604     C                   KFLD                    cldDEP
042800160301      **
042900050414      *------------------
043000020919      * Apro file transito o FiVAB pilotato dalla tabella
043100991124      * Caricamento Tabella PESI DPD
043200060620     c                   clear                   XX
043300991124     c                   clear                   DPP
043400991124     c     'DPP'         chain     tntbe000                           31
043500991124      *
043600991124     c                   DOW       *in31 = *off
043700991124      * Se il S.I. è indicato deve essere uguale al mio
043800991124     c                   IF        TBEatb = *BLANKS  and
043900991129     c                              (TBEsif = Knsif  or  TBEsif = *blanks)
044000991124     c                   add       1             xx
044100991124     c                   movel     TBEke1        DPPK(XX)
044200000406     c                   movel     TBEuni        DDPP
044300000410     c                   movel     §DPPpkb       DPP(XX)
044400141210     c                   movel     §DPPvlb       vlb(XX)
044500991124     c                   endif
044600991124     c     'DPP'         reade     tntbe000                               31
044700991124     c                   enddo
044800160318      *
044900160318     c                   movel(p)  'NSD'         tbeCOD
045000160318     c                   movel(p)  '1'           tbeKE1
045100160318     C     KTBE          chain     tntbe01l
045200160318     c                   if        %Found(tntbe01l)  and TBEatb = *BLANKS
045300160318     c                   eval      dNSD = tbeUNI
045400160318     c                   end
045500160317      *
045600160317     c                   z-add     0             vers_CAP_DPD      5 0
045700160317     c* recupero l'ultima versione di cappario DPD
045800160317     c                   clear                   tisie2ds
045900160317     c                   eval      sie2dri=*all'9'
046000160317     c                   call      'TISIE2R'
046100160317     c                   parm                    tisie2ds
046200160317     c* se trovata versione reperisco  descrizioni deposito
046300160317    2c                   if        sie2ver > 0
046400160317     c                   z-add     sie2ver       vers_CAP_DPD
046500160317     c                   end
046600160317      *
046700160317      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
046800160317     c                   movel     kpjbu         svkpjbu
046900160317     C                   CLEAR                   TRUL06DS
047000160317     C                   MOVE      '£1'          D06COD
047100160317     C                   MOVEL     Filiale       D06KEY
047200160317     C                   MOVEL     'L'           D06TLA
047300160317     C                   MOVEL     TRUL06DS      KPJBU
047400160317     C                   CALL      'TRUL06R'
047500160317     C                   PARM                    KPJBA
047600160317     c                   MOVEL     KPJBU         TRUL06DS
047700160317     c                   eval      kpjbu = svkpjbu
047800000908      *
047900971216      * Recupero data e ora
048000971216     C                   TIME                    WORA
048100991124     C                   TIME                    W0140
048200991124      * UDATE IN GGMMAAAA
048300991124     C                   MOVE      W0140         WDTGIO
048400991124      * UDATE IN AAAAMMGG
048500991124     C     *eur          MOVEL     WDTGIO        DATA_eur
048600991124     C     *iso          MOVEL     DATA_eur      dateu
048700141103      *-------------
048800991124     C                   ENDSR
053400060614      *-------------------------------------------------------------------------
053500160317      *  in base al Depot di provenienza determina Linea e Cliente
053600160317      *----------------------------------------------------------------
053700160317     C     LNP_da_dove   BEGSR
053800160318      **
053900160318      * Default
054000160318      ** aggancia i dati della tab.LPD  di default
054100160318     c                   if          Linea_Partenza = 0
054200160318     c                   z-add     190           Linea_Partenza    3 0
054300160318     c                   end
054400160317      *
054500160317      *  con il DEPOT di provenienza  aggancia x Versione il Cappario DPD
054600160317      *   e se non fosse presente con i primi 4 caratteri del Parcel
054700160317      *
054800160317     C     key_Depot     chain     DPcdp01l
054900160317     c                   if        %Found(DPcdp01l)
055000160317      *
055100160317     C     key_Dep       setgt     DPpdc01l
055200160317     C     DepotParcel   readpe    DPpdc01l
055300160317     c                   dow       not %Eof(DPpdc01l)
055400160317      *   se trovato il Depot VALIDO
055500160317     c                   if        pdcDDE <= dateU and dateU <= pdcDSC
055600160317     c                   eval      DpdcFLO = pdcFLO
055700160317      *
055800160317     c                   if        §PDCFLOLNP <> *blank  and
055900160318     c                             §PDCFLOLNP <> '000'   and
056000160318     c                             %check(digits:§PDCFLOLNP)=0
056100160318     c                   move(p)   §PDCFLOLNP    linea_PARCEL      3 0
056200160318      **
056300160318      **  controlla se appartiene alle linee della £1
056400160318     c     linea_PARCEL  lookup    LIN                                    25
056500160318     c                   if        %Found
056600160317     c                   move      §PDCFLOLNP    linea_partenza    3 0
056700160318     c                   leave
056800160318     c                   end
056900160318      **
057000160317     c                   end
057100160317     c                   end
057200160317      *
057300160317     C     DepotParcel   readpe    DPpdc01l
057400160317     c                   Enddo
057500160317     c                   End
057600160318      *
057700160317     c                   Endsr
057800160317     C*----------------------------------------------------------------
057900160317     C*  Aggancia tab.LPD ed imposta i campi del VAB
058000160317     C*----------------------------------------------------------------
058100160317     C     tabella_LPD   BEGSR
058200160317      *
058300160317     c                   clear                   FIEULPDds
058400160318      *
058500160317     c                   movel(p)  linea_partenzaiLPDlnp
058600160318     c                   movel(p)  linea_partenzaiLPDlna
058700160317     c                   movel(p)  nrParcel      iLPDparcel
058800160318      *
058900160317     c                   call      'FIEULPDR'
059000160317     c                   parm                    FIEULPDds
059100160317      *
059200160317      * ha trovato i dati necessari per instradare il record della spedizione
059300160317      * alla filiale di partenza
059400160318     c                   clear                   dLPD
059500160318     c                   if         oLPDtip <> 'E'
059600160318     c                   eval           dLPD = oLPDUNI
059700160616     C***                eval      su_EDIVAB = §LPDWRK
059800160616     C                   eval         §lpccm = §LPDKSC
059900160616     C                   eval         §lpctm = §LPDCTM
060000160330      *
060100160330      ** tariffa non sarebbe corretta
060200160616     C                   eval         §lpCTR = OLPDTAR
060300160616     C                   movel     linea_partenza§lpLNP
060400160616     C                   movel     linea_partenza§lpFGS
060500160317     c                   end
060600160317      *
060700160317      *  QUI la tariffa LHS non deve essere considerata poichè posso conoscere
060800160317      *  la linea di PARTENZA ma la linea di ARRIVO non la conosco.
060900160317      *
061000160317     c                   Endsr
061001160616      *---------------------------------------------------------
061002160616      * Controllo i caratteri dei campi alfanumerici
061003160616      *---------------------------------------------------------
061004160616     c     Sr_Chk        BegSr
061005160616
061006160616     c                   Eval      TxtInOut = wtxt
061007160616     c                   Call      'XCHKCHAR'
061008160616     c                   Parm                    TxtInOut
061009160616     c                   Parm                    ElencoChar
061010160616     c                   Parm                    TipoElenco
061011160616     c                   Parm                    CharSost
061012160616     c                   Parm                    UpperCase
061013160616     c                   Parm                    ChkNull
061014160616     c                   Parm                    CharNull
061015160616     c                   Parm                    Esito
061016160616     c                   If        Esito = '1'
061017160616     c                   Move      TxtInOut      wtxt
061018160616     c                   EndIf
061019160616
061020160616     c                   EndSr
061100160317     C*----------------------------------------------------------------
