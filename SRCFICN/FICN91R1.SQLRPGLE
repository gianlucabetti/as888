000001090211     H DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP(*CALLER)
002107090210     Ffitgt01l  uF   E           K DISK    commit
002108090625     Ffiadt01l  uF   E           K DISK    commit
002109090211     Ftabel00f  iF   E           K DISK
002110090211     Ffiapd01l  iF   E           K DISK
002111090213     Fazorg01l  iF   E           K DISK
002112090211     FFicn91p1  O    E             PRINTER OFLIND(*IN80) usropn
002113090213     d commanl         s            500
002114090213     d lung            s             15  5
002115090210     d scadenza        s              8  0
002116090210     d oggi            s              8  0
002117090210     d oggim2m         s              8  0
002118090210     d dataiso         s               d
002119090210     d dataeur         s               d   datfmt(*eur)
002120090211     d ddc             s                   like(tgtddt)
002121090211     d stpddt          s                   like(tgtddt)
002122090211     d stpdsc          s                   like(tgtddt)
002123090211     d po              s              3  0
002124090211     d posav           s              3  0
002125090213     d* gg pulizia
002126090211     d ds5a          e ds
002127090213     d trtcm1ds      e ds
002128090211     d* Parametri x Controllo profilo utenti
002129090211     d TIBS34ds      e ds
002130090211     d AZUTEds       e ds                  extname(AZUTE00F)
002131090211     d* Ds per dati organigramma
002132090211     d DDatiUte      e ds
002133090213     d Og148         e ds                  inz
002134090213     D CMD9            S             48    DIM(1) CTDATA PERRCD(1)
003900951009     D KPJBA         E DS
003901090624?     *--------------------------------------------------------------*
003902090624     c* città
010701090209     c     *loval        setll     fitgt01l
010702071012     c                   do        *hival
010703090209     c                   read      fitgt01l
010704090209     c                   if        %eof(fitgt01l)
010705071012     c                   leave
010706071012     c                   end
010707090211     c* non elabora quelle annullate
010708090211     c                   if        tgtatb <>' '
010709090209     c                   iter
010710090209     c                   end
010711090211     c* non elabora quelle di simulazione
010712090211     c                   if        tgtsml = 'S'
010713090211     c                   iter
010714090211     c                   end
010715090211     c* non elabora quelle manutenzionate negli ultimi 2 mesi
010716090211     c                   if        tgtduv > oggim2m
010717090209     c                   iter
010718090209     c                   end
010719090211     c* non elabora quelle ancora da stampare
010720090211     c                   if        tgtdts = 0
010721090211     c                   iter
010722090211     c                   end
010723090211     c* non elabora quelle ancora da confermare
010724090211     c                   if        tgtdcn = 0
010725090211     c                   iter
010726090211     c                   end
010727090624     c* non elabora quelle che scadono il 31122039
010728090212     c                   if        tgtdst <> 20391231
010729090211     c                   iter
010730090211     c                   end
010731090211     c* non elabora quelle che hanno decorenza successiva
010732090211     c                   if        tgtddt >= scadenza
010733090211     c                   iter
010734090211     c                   end
010735090209     c* verifico se ho valorizzazioni e se sono + vecchie di due mesi
010736090211     c                   clear                   ddc
010737090211     C/EXEC SQL
010738090211     C+ SELECT max(fttddc) INTO :ddc FROM fiftt00f WHERE Fttpdr = :tgtpdr
010739090211     C/END-EXEC
010740090211     c                   if        ddc > oggim2m
010741090211     c                   iter
010742090211     c                   end
010743090211     c* se la data distinta è compresa nel range di validità della tariffa
010744090211     c* oppure se non ho valorizzazioni, la faccio scadere
010745090211     c                   if        ddc = 0 or
010746090211     c                             (ddc <> 0 and ddc >=tgtddt and
010747090211     c                             ddc < tgtdst)
010748090211     c                   eval      tgtdst = scadenza
010749090209     c                   eval      tgtduv = oggi
010750090209     c                   update    fitgt000
010751090331     c                   eval      comcom = *on
010752090209     c                   call      'FICN03R'
010753090209     c                   parm                    tgtpdr
010754090209     c                   parm                    tgtsml
010755090209     c                   parm                    tgtprg
010756090331     c                   parm                    comcom            1
010757090209     c                   commit
010758090209     c                   end
010759090209     c                   enddo
010760090624     c* aff/defl
010762090624     c     *loval        setll     fiadt01l
010763090624     c                   do        *hival
010764090624     c                   read      fiadt01l
010765090624     c                   if        %eof(fiadt01l)
010766090624     c                   leave
010767090624     c                   end
010768090624     c* non elabora quelle annullate
010769090624     c                   if        adtatb <>' '
010770090624     c                   iter
010771090624     c                   end
010776090624     c* non elabora quelle manutenzionate negli ultimi 2 mesi
010777090624     c                   if        adtduv > oggim2m
010778090624     c                   iter
010779090624     c                   end
010780090624     c* non elabora quelle ancora da stampare
010781090624     c                   if        adtdts = 0
010782090624     c                   iter
010783090624     c                   end
010784090624     c* non elabora quelle ancora da confermare
010785090624     c                   if        adtdcn = 0
010786090624     c                   iter
010787090624     c                   end
010788090624     c* non elabora quelle che scadono il 31122039
010789090624     c                   if        adtdst <> 20391231
010790090624     c                   iter
010791090624     c                   end
010792090624     c* non elabora quelle che hanno decorenza successiva
010793090624     c                   if        adtddt >= scadenza
010794090624     c                   iter
010795090624     c                   end
010796090624     c* verifico se ho valorizzazioni e se sono + vecchie di due mesi
010797090624     c                   clear                   ddc
010798090624     C/EXEC SQL
010799090624     C+ SELECT max(attddc) INTO :ddc FROM fiatt00f WHERE attpdr = :adtpdr
010800090624     C/END-EXEC
010801090624     c                   if        ddc > oggim2m
010802090624     c                   iter
010803090624     c                   end
010804090624     c* se la data distinta è compresa nel range di validità della tariffa
010805090624     c* oppure se non ho valorizzazioni, la faccio scadere
010806090624     c                   if        ddc = 0 or
010807090624     c                             (ddc <> 0 and ddc >=adtddt and
010808090624     c                             ddc < adtdst)
010809090624     c                   eval      adtdst = scadenza
010810090624     c                   eval      adtduv = oggi
010811090624     c                   update    fiadt000
010818090624     c                   commit
010819090624     c                   end
010820090624     c                   enddo
010821090706     c************************************************************
010822090624     c* rileggo le testate che ho fatto scadere e mando mail a POC
010823090706     c************************************************************
010824090624     c                   clear                   compdrd
010825090624     c                   movel     9999999       compdra
010826090624     c     *loval        setll     azorg01l
010827090210     c                   do        *hival
010828090624     c                   read      azorg01l
010829090624     c                   if        %eof(azorg01l)
010830090210     c                   leave
010831090210     c                   end
010832090624     c*
010833090624     c                   if        ORGFVA <> ' ' or
010834090624     c                             ORGFAG <> 'F'
010835090624     c                   iter
010836090624     c                   end
010837090624     c                   movel     orgfil        compdrd           7 0
010838090624     c                   movel     orgfil        compdra           7 0
010839090625     c* città
010840090625     c     compdrd       setll     fitgt01l
010841090624     c                   do        *hival
010842090624     c                   read      fitgt01l
010843090624     c                   if        %eof(fitgt01l)
010844090624     c                   leave
010845090624     c                   end
010846090624     c                   movel     tgtpdr        po
010847090625     c*
010848090625     c                   if        po <> orgfil
010849090625     c                   leave
010850090625     c                   end
010851090625     c*
010852090625     c                   eval      tipos = 'C'
010853090210     c* non elabora quelle di simulazione
010854090210     c                   if        tgtsml = 'S'
010855090210     c                   iter
010856090210     c                   end
010857090210     c* non elabora quelle ancora da confermare
010858090210     c                   if        tgtdcn = 0
010859090210     c                   iter
010860090210     c                   end
010861090210     c* solo quelle con scadenza = a quella dell'elaborazione
010862090211     c                   if        tgtdst <> scadenza
010863090210     c                   iter
010864090210     c                   end
010865090210     c* solo quelle aggiornate oggi
010866090210     c                   if        tgtduv <> oggi
010867090210     c                   iter
010868090624     c                   end
010869090624     c                   if        not %open(ficn91p1)
010870090624     c                   exsr      sropen
010872090210     c                   end
010880090211     c                   exsr      srstampa
010881090210     c                   enddo
010882090625     c* aff/defl
010883090625     c     compdrd       setll     fiadt01l
010884090625     c                   do        *hival
010885090625     c                   read      fiadt01l
010886090625     c                   if        %eof(fiadt01l)
010887090625     c                   leave
010888090625     c                   end
010889090625     c                   movel     adtpdr        po
010890090625     c*
010891090625     c                   if        po <> orgfil
010892090625     c                   leave
010893090625     c                   end
010894090625     c*
010895090625     c                   eval      tipos = 'A'
010896090625     c* non elabora quelle ancora da confermare
010897090625     c                   if        adtdcn = 0
010898090625     c                   iter
010899090625     c                   end
010900090625     c* solo quelle con scadenza = a quella dell'elaborazione
010901090625     c                   if        adtdst <> scadenza
010902090625     c                   iter
010903090625     c                   end
010904090625     c* solo quelle aggiornate oggi
010905090625     c                   if        adtduv <> oggi
010906090625     c                   iter
010907090625     c                   end
010908090625     c                   if        not %open(ficn91p1)
010909090625     c                   exsr      sropen
010910090625     c                   end
010920090625     c                   exsr      srstampa
010921090625     c                   enddo
010922090625     c*
010923090625     c                   if        %open(ficn91p1)
010924090625     c                   close     ficn91p1
010925090625     c                   seton                                        8001
010926090625     c                   end
010927090625     c                   enddo
012200951009     C*
012201071012     c                   seton                                        lr
020800951011?     *--------------------------------------------------------------*
020900090213?     *  open file di stampa per mandare mail
021000951011?     *--------------------------------------------------------------*
021100090213     C     sropen        BEGSR
021101090213     c                   clear                   trtcm1ds
021102090213     c                   clear                   og148
021103090624     c                   move      orgfil        poa               3
021106090213     C                   MOVEL     ORGDE8        OG148
021108090213     c                   if        §oglpo ='1'
021109090213     c                   eval      §cm1dst = 'RP1' + poa
021110090213     c                   else
021111090213     c                   eval      §cm1dst = 'RP2' + poa
021112090213     c                   end
021115090213     c                   movel     'C01'         §cm1tips
021125090213     c                   eval      §cm1var = '*OBJM*' + 'Tariffe scadute -
021126090213     c                             per inutilizzo'
021166090213     c                   eval      commanl = %trim(Cmd9(1))+ trtcm1ds + ''')'
021167090213     c                   eval      lung = %len(%trim(commanl))
021168090706     C*                  CALL      'QCMDEXC'                            60
021169090706     C*                  PARM                    COMMANl         500
021170090706     C*                  PARM                    LUNG             15 5
021171090213     C                   OPEN      ficn91p1
021172090213     c                   endsr
021173090213?     *--------------------------------------------------------------*
021174090213?     *  stampa elenco tariffe scadute
021175090213?     *--------------------------------------------------------------*
021176090213     C     srstampa      BEGSR
021177090625     c                   if        tipos = 'C'
021178090211     c                   move      tgtddt        dataiso
021179090211     c                   move      dataiso       dataeur
021180090211     c                   move      dataeur       stpddt
021181090211     c                   move      tgtdst        dataiso
021182090211     c                   move      dataiso       dataeur
021183090211     c                   move      dataeur       stpdsc
021184090625     c                   eval      apdtip = 'A'
021185090625     c                   else
021186090625     c                   move      adtddt        dataiso
021187090625     c                   move      dataiso       dataeur
021188090625     c                   move      dataeur       stpddt
021189090625     c                   move      adtdst        dataiso
021190090625     c                   move      dataiso       dataeur
021191090625     c                   move      dataeur       stpdsc
021192090625     c                   eval      tgtpdr = adtpdr
021193090625     c                   eval      tgtprg = adtprg
021194090625     c                   clear                   tgtdcv
021195090625     c                   eval      apdtip = 'D'
021196090625     c                   end
021197090211     c     kapd          chain     fiapd01l
021198090211     c                   if        not %found(fiapd01l)
021199090211     c                   eval      apdrsf = *all'?'
021200090211     c                   end
021201090211     c*
021202090211     c                   if        *in80
021203090211     c                   write     testa
021204090211     c   01              write     testa1
021205090211     c                   setoff                                       0180
021206090211     c                   write     testa2
021207090211     c                   end
021208090211     c                   write     det
021209090211     c*
021210090211     C                   ENDSR
021211090211?     *--------------------------------------------------------------*
021212090211?     *  *INZSR: Operazioni di inizializzazione dati                 *
021213090211?     *--------------------------------------------------------------*
021214090211     C     *INZSR        BEGSR
021215951011      *
021300951011     C     *ENTRY        PLIST
021400951011     C                   PARM                    KPJBA
021401090210     c     ktab          klist
021402090211     c                   kfld                    tblkut
021403090210     c                   kfld                    tblcod
021404090210     c                   kfld                    tblkey
021405090211     c     kapd          klist
021406090211     c                   kfld                    apdtip
021407090211     c                   kfld                    tgtpdr
021411090211     c                   eval      tblkut = 1
021412090210     c                   eval      tblcod = '5A'
021413090210     c                   eval      tblkey = '1'
021414090211     c                   eval      nompgm = 'FICN91R1'
021415090211      * Reperisco dati job
021416090211     c                   exsr      DatiJob
021418090210     c*
021419090210     c     ktab          chain     tabel00f
021420090211     c                   if        %found(tabel00f) and tblflg = ' '
021421090210     c                   movel     tbluni        ds5a
021422090210     c                   else
021423090210     c                   movel     *hival        §5AL1
021424090210     c                   movel     *hival        §5AFGT
021425090210     c                   end
021426090210     c*
021427090210     C                   TIME                    W0120            14 0
021428090210     C                   MOVE      W0120         WDAT              8 0
021429090210     c*
021430090211     c                   move      wdat          woggi             8 0
021431090211     c                   movel     w0120         times             6 0
021432090211     c                   move      wdat          dataeur
021433090210     c                   move      dataeur       dataiso
021434090210     c                   move      dataiso       oggi
021435090211     c     dataiso       subdur    §5AFGT:*d     dataiso
021436090211     c                   move      dataiso       oggim2m
021437090211     c                   move      wdat          dataeur
021438090211     c                   move      dataeur       dataiso
021439090211     c     dataiso       adddur    §5Al1:*d      dataiso
021440090211     c                   move      dataiso       scadenza
021441090625     c                   seton                                        8001
022500951012     C                   ENDSR
022600090211      *---------------------------------------------------------------*
022700090211      * Reperimento Dati del job (Utente/Operativi)                   *
022800090211      *---------------------------------------------------------------*
022900090211     c     DatiJob       BEGSR
023000090211      *
023100090211     c     *dtaara       define    §azute        azuteds
023200090211     c     *dtaara       define    §datiute      ddatiute
023300090211      *
023400090211     c                   in(E)     *dtaara
023500090211     c                   IF        %ERROR or RSUT = *blanks
023600090211     c                   clear                   Tibs34Ds
023700090211     c                   call      'TIBS34R'
023800090211     c                   parm                    Tibs34Ds
023900090211     c                   in        *dtaara
024000090211     c                   ENDIF
024100090211      *
024200090211     c                   ENDSR
024300090213**         CMD9
024400090213OVRPRTF FILE(FICN91P1) OUTQ(EMAILIN) USRDFNDTA('
