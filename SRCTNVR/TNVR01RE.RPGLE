000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000301160616      ***
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
014200160317      * ?CAP
014300160826     c                   move      '40050'       field5            5            C.A.P.
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
020100160316      *
021000160826     c                   z-add     15            §lpPKB
021100160826     c                   z-add     0,01          §lpVLB
021300160316      * ?Codice bolla
021400160826     C                   MOVEL     'F*'          §lpcbo                         NO C/assegno
021500160317      * ?Provenienza
021600160616     c******             movel     '1'           vabscl
021601160616     c                   move      *all'9'       §lppoe
021602160616     c                   move      *all'9'       §lpnsr
021603160616     c                   move      *all'9'       §lpnor
021604160616     c                   move      *all'9'       §lpnrv
021700160316      *
022400160316      *
023900160317      **
024000160317      *--------
024600160826     c                   move      '192'         §lplnp
024601160826     c                   move      '001'         §lplna
025100160318      **
025400160318      *
025700160317      *
025701160826     c                   eval      §LPRSM='DPD11111111'
025702160826     c                   eval      §LPINM='DPD1111111111111111'
025703160826     c                   eval      §LPCAM='26140'
025704160826     c                   eval      §LPLOM='SAINT RAMBERT D ALBON (E'
025706160826     c                   eval      §LPNZM='F'
025707160826
025708160826
025800160318      * da tabella NSD (1)
025900160826     c                   movel     'Prova'       §lpRSD
026000160826     c                   movel     'prova1234567'§lpIND
026100160826     c                   movel     'ARGELATO'    §lpLOD
026200160318      *
026300160318      *
028301160616     c* per il momento imposto ccn = ccm
028302160826     c                   eval      §lpctm='4F'
028303160826     c                   eval      §lpccm=1929999
028304160826     c                   eval      §lpccn=1929999
028305160616     c                   eval      §lpdrt=dateu
028306160616     c                   eval      §LPFPP='P'
028311160826     c                   z-add     1920999       §lppdr
028313160616     c                   eval      §lpfpr='D'
028314160826     c                   eval      §lpsop='*'
028315160909     c****               eval      §lpnpr='08448703301857'
028316160909     c****               eval      §lprmn=08448703301857
028317160826     c                   movel     dateu         §lpaas
028318160826     c                   move      dateu         §lpmgs
028319160616
028320160616     C                   CALL      'FNLS01R'
028321160616     C                   PARM                    KPJBA
028322160616     C                   PARM                    DBLP
028323160616     C                   PARM                    DTASV
028324160616     C                   PARM                    trul90ds
028400060612      *
028500160317     c                   Return
038300051205      *----------------------------------------------------*
038400051205      *   DEFINIZIONE CHIAVI                               *
038500051205      *----------------------------------------------------*
038600051205     C     *INZSR        BEGSR
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
046600160317      *
046700160317      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
046800160317     c                   movel     kpjbu         svkpjbu
046900160317     C                   CLEAR                   TRUL06DS
047000160317     C                   MOVE      '£1'          D06COD
047100160826     C                   MOVEL     001           D06KEY
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
