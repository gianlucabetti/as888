000100050512      *PARMS CLOSQLCSR(*ENDMOD)
000200050512      *PARMS DYNUSRPRF(*OWNER)
000300041123     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000400041124
000500050512     H* TNTA91R *-----------------------------------------------------*
000600050512     H*         - Elenco clienti per sottoconto intestazione fattura  *
000700970225     H*---------------------------------------------------------------*
000800050513     FCNCLP02l  IF   E           K DISK    prefix(F_)
000900050513     Ftabel00f  IF   E           K DISK
001000051129     Ffnspe03l  if   e           k disk
001100051129     Ffnsp201l  if   e           k disk
001200051129     Ftfntc01l  if   e           k disk
001300061002     Fwfta911l  uf a e           k disk    usropn
001400050513     FTNTA91P   O    E             PRINTER oflind(*In99)
001500041116
001600050513     D CFF             S              1  0 DIM(15)
001700050513     D DFF             S             30    DIM(15)
001800050516     d pog             s              3    dim(250)
001900050516     d wabi            s                   like(UteAut)
002000050513     d dsff          e ds
002100051129     d dtft          e ds
002200051129     d ds13          e ds
002300060929     d ds01          e ds
002400050516     d tnta90ds        ds
002500050516     d  v1cpop                 1      3
002600050516     d  d90abl                 4      4
002700051130     d  d90fatt                5      5
002800051130     d  d90anom                6      6
002900060929     d* ordinamento K=cliente; C=commerciale
003000060929     d  v1cord                 7      7
003100970224     D KPJBA         E DS
003200041116     d Azuteds       e ds                  Extname(Azute00f)
003300041116     d dDatiute      e ds
003400041116     d Tibs34ds      e ds                  Inz
003500050516     d dute01        e ds
003600050516     d trul31ds      e ds
003700050513     D* DS PER TIBS02R - GESTIONE TNTBE00F
003800050513     D DSBS02        E DS                  EXTNAME(TIBS02DS)
003900050513     d cnclpxxDs     e ds                  ExtName(cnclp00f)
004000050513     d TIBS69DS      E DS                  INZ
004100050513     d DS_cnaco      E DS                  extname(CNACO00F)
004200050513     d DS_cnind      E DS                  extname(CNIND00F)
004300050513     d DS_cnclp      E DS                  extname(CNCLP00F)
004400050513     d                                     prefix(F_)
004500050513     d DS_fncls      E DS                  extname(FNCLS00F)
004600050513     d
004700051201     D WrkGetListaclp  S           4500
004800050513     D                                     VARYING
004900051201     D WrkclpFilIn     S           4000
005000050513     D                                     VARYING
005100050513     d i               S              5I 0
005200041124
005300050512     D codut           S              1  0 inz(1)
005400050513     D keytab          S                   like(tblkey)
005500050513     d w_clpscf        s                   Like(clpscf)
005600051130     d s_clpscf        s                   Like(clpscf)
005700051129     d wksc            s                   Like(acoksc)
005800060929     d sav_age         s                   Like(clpage)
005900050513     d $righe          s              2  0
006000051129     D Kapl            s                   LIKE(NTCapl) inz('C')
006100051129     D Knk1            s                   LIKE(NTCnk1)
006200051129     D Knk2            s                   LIKE(NTCnk2)
006300051129     D Ktnt            s                   LIKE(NTCtnt) inz('84')
006400051129     D Kfls            s                   LIKE(spefls) inz('L')
006500051129     D Kcod            s                   LIKE(specod) inz('001')
006600051129     D comtpe          s                   like(sp2tpe) inz('EM')
006700051130     D TITfatt         C                   CONST('         ELENCO CLIENTI PER S-
006800051130     D                                     OTTOCONTO INTESTAZIONE FATTURA SENZA-
006900051130     D                                      INVIO FATTURA VIA E-MAIL         ')
007000051130     D TITanoma        C                   CONST('           ELENCO CLIENTI PER-
007100051201     D                                      SOTTOCONTO INTESTAZIONE FATTURA CON-
007200051130     D                                      ANOMALIE INVIO FATTURA           ')
007300051130     D TITolo          C                   CONST('E L E N C O   C L I E N T I  -
007400051130     D                                      P E R   S O T T O C O N T O   I N T-
007500051130     D                                      E S T A Z I O N E   F A T T U R A')
007600041124
007700970228      *---------------------------------------------------------------*
007800050512     c* Eseguo sql per estrarre da cnclp solo i dati che mi servono
007900050512     c                   exsr      sr_Sqlclp
008000050512     c* Lettura ed elaborazione di cnclp
008100051130     c                   if        d90fatt <> 'S' and d90anom <> 'S'
008200050513     c                   exsr      sr_Leggiclp
008300051130     c                   else
008400051130     c                   exsr      sr_Leggiclp1
008500051130     c                   endif
008600060929     c* se richiesto ordinamento per codice comm.le stampo leggendo il file
008700060929     c* di work
008800060929     c                   if        v1cord='C'
008900060929     c                   clear                   sav_age
009000060929     c     *loval        setll     wfta911l
009100060929     c                   do        *hival
009200060929     c                   read      wfta911l
009300060929     c                   if        %eof(wfta911l)
009400060929     c                   leave
009500060929     c                   endif
009600060929     c                   z-add     wta91scf      clpscf
009700060929     c                   z-add     wta91age      clpage
009800060929     c                   exsr      sr_elabora
009900060929     c                   enddo
010000060929     c                   endif
010100050512     c
010200050512     c                   seton                                        lr
010300050513     c                   write     fines
010400051130     c                   write     rcd007
010500050513     c*
010600050513     C                   eval      I69TLA  = 'C'
010700050513     C                   call      'TIBS69R'
010800050513     C                   parm                    TIBS69DS
010900050512     c**********************************************************************
011000050512     c     Sr_Sqlclp     begsr
011100050513     **?Imposto la parte fissa dell'istruzione.
011200050513     C                   EVAL      WrkGetListaCLP =
011300050513     C                             'SELECT * from cnclp00f'
011400050513     **?WHERE
011500050513     C                   EVAL      WrkGetListaCLP = WrkGetListaCLP +
011600050516     C                             ' WHERE clpscf > 0 '
011700050513     **?Selezione dei codici appartenenti ai po richiesti
011800050513     C                   EXSR      WhrPOArea
011900050513     **?Selezione su codice utente e capoconto
012000050513     C                   EVAL      WrkGetListaCLP = WrkGetListaCLP
012100050513     C                             +
012200050513     C                             ' AND clpkut = ' + %triml(%editc(codut:'Z'))
012300050513     c                             +
012400050513     c                             ' and clpkcc = ' + %triml(%editc(dutkci:'Z'))
012500050513     C                   EVAL      WrkGetListaCLP = WrkGetListaCLP
012600050513     C                             + ' ORDER BY clpkut, clpkcc, clpscf '
012700050513     **?Solo lettura.
012800050513     C                   EVAL      WrkGetListaCLP = WrkGetListaCLP +
012900050513     C                             ' FOR READ ONLY'
013000050513     C/EXEC SQL
013100050513     C+ PREPARE ListaCLP FROM :WrkGetListaCLP
013200050513     C/END-EXEC
013300050513     C
013400050513     C/EXEC SQL
013500050513     C+ DECLARE ListaCLP CURSOR FOR ListaCLP
013600050513     C/END-EXEC
013700050513     C
013800050513     C/EXEC SQL
013900050513     C+ OPEN ListaCLP
014000050513     C/END-EXEC
014100050512     c                   endsr
014200050513     ***********************************************************************
014300050513     **?
014400050513     **?Stringa WHERE con i PO richiesti
014500050513     **?
014600050513     ***********************************************************************
014700050513     C     WhrPOArea     BEGSR
014800050513     C                   RESET                   WrkClpFilIn
014900050513     **?Reperisco i PO che compongono l'area amministrativa.
015000050513
015100050513    1C                   DO        250           i
015200050513
015300050513    2C                   IF        POG(i) = '000' or pog(i) = '   '
015400050513     C
015500050513     C                   LEAVE
015600050513     C
015700050513   x2C                   ELSE
015800050513     **?L'utente è autorizzato al PO, compongo la stringa IN.
015900050513    3C                   SELECT
016000050513     C                   WHEN      %LEN(WrkClpFilIn) = 0
016100050513     C                   EVAL      WrkClpFilIn = ' AND (substr(digits(clpscf),-
016200050513     C                              1, 3) in (''' + pog(i) + ''''
016300050513     C                   WHEN      %LEN(WrkClpFilIn) > 0
016400050513     C                   EVAL      WrkClpFilIn = WrkClpFilIn
016500050513     C                             + ' , ''' +  pog(i) + ''''
016600050513    3C                   ENDSL
016700050513     C
016800050513    2C                   ENDIF
016900050513
017000050513    1C                   ENDDO
017100050513     c*  Aggiungo parentesi a fine istruzione IN
017200050513     C                   EVAL      WrkClpFilIn = WrkClpFilIn + ')'
017300050513
017400050513    1C                   DO        250           i
017500050513
017600050513    2C                   IF        POG(i) = '000' or pog(i) = '   '
017700050513     C
017800050513     C                   LEAVE
017900050513     C
018000050513   x2C                   ELSE
018100050513     **?L'utente è autorizzato al PO, compongo la stringa IN.
018200050513    3C                   SELECT
018300050513     C                   WHEN      i = 1
018400050513     C                   EVAL      WrkClpFilIn = wrkClpFilIn
018500050513     C                             + ' OR substr(digits(clpksc), 1, 3) in('''
018600050513     C                             + pog(i) + ''''
018700050513     C                   WHEN      %LEN(WrkClpFilIn) > 1
018800050513     C                   EVAL      WrkClpFilIn = WrkClpFilIn
018900050513     C                             + ' , ''' + pog(i) + ''''
019000050513    3C                   ENDSL
019100050513     C
019200050513    2C                   ENDIF
019300050513
019400050513    1C                   ENDDO
019500050513     c*
019600050513     c* aggiungo )) a fine istruzione in
019700050513     C                   EVAL      WrkClpFilIn = WrkClpFilIn + '))'
019800050513
019900050513     C                   EVAL      WrkGetListaClp = WrkGetListaClp
020000050513     C                             + WrkClpFilIn
020100050513     C                   ENDSR
020200050512     c**********************************************************************
020300050513     c     Sr_LeggiClp   begsr
020400050513     c                   clear                   w_clpscf
020500050512     c*
020600050513     C/EXEC SQL
020700050513     C+ Fetch listaclp into :cnclpxxds
020800050513     C/END-EXEC
020900051129    1c                   Dow       Sqlcod = 0
021000050513     c* a rottura di sottoconto intestazione fattura rileggo cnclp per
021100050513     c* stampare tutti i clienti appartenenti allo stesso gruppo
021200050513
021300050513     c* scarto record se scf=ksc perchè devo andare in stampa solo se presen
021400050513     c* te più di un cliente
021500051129    2c                   if        clpscf <> clpksc
021600050516     c* se richiesti gli unificati non bloccati scarto record se
021700050516     c* unificato bloccato
021800051129    3c                   if        clpscf <> w_clpscf
021900051129     c                   exsr      sr_welab
022000051129    4c                   if        w_elab = ' '
022100060929     c* se richiesto ordinamento per sottoconto int.fattura stampo altriment
022200060929     c* se richiesto ordinamento per comm.le memorizzo in workfile
022300060929     c* sottoc.int.fatt./codice comm.le
022400060929     c                   if        v1cord='K'
022500050513     c                   exsr      sr_elabora
022600060929     c                   else
022700060929     c                   exsr      wrt_wf
022800060929     c                   endif
022900050513     c                   eval      w_clpscf = clpscf
023000051129    4c                   endif
023100051129    3c                   endif
023200051129    2c                   endif
023300050513     C/EXEC SQL
023400050513     C+ Fetch listaclp into :cnclpxxds
023500050513     C/END-EXEC
023600050513
023700051129    1c                   EndDo
023800050513     c*
023900050512
024000050513     C/EXEC SQL
024100050513     C+ close listaclp
024200050513     C/END-EXEC
024300050512     c                   endsr
024400051130     c**********************************************************************
024500051130     c     Sr_leggiclp1  begsr
024600051130     c                   clear                   w_clpscf
024700051130     c                   clear                   s_clpscf
024800051130     c*
024900051130     C/EXEC SQL
025000051130     C+ Fetch listaclp into :cnclpxxds
025100051130     C/END-EXEC
025200051130    1c                   Dow       Sqlcod = 0
025300051130     c                   if        clpscf <> s_clpscf
025400051130     c                   clear                   wscf              1
025500051130     c                   z-add     clpscf        s_clpscf
025600051130     c                   endif
025700051130     c                   if        clpscf <> w_clpscf
025800051130     c* se richiesti gli unificati non bloccati scarto record se
025900051130     c* unificato bloccato
026000051130     c                   exsr      sr_welab
026100051130     c                   clear                   wprt              1
026200051130    2c                   if        w_elab = ' '
026300051130     c* letto codice non bloccato verifico in base alle selezioni se
026400051130     c* cliente da stampare
026500051130     c* verifico se raggruppamento da stampare
026600051130     c                   z-add     acoksc        wksc
026700051130     c                   exsr      sr_spenota
026800051130    3c                   select
026900051130     c* RICHIESTA STAMPA CLIENTI NO E-MAIL
027000051130     c                   when      d90fatt = 'S'
027100051130    4c                   select
027200051130     c* se non tovato ne' luogo ne' nota verifico il sottoconto intestaz.
027300051130     c* fattura
027400051130     c                   when      wfnota = *blanks  and wfluo=*blanks
027500051130     c                   if        clpscf = wksc
027600051130     c                   eval      wprt  = 'S'
027700051130     c                   else
027800051130     c                   if        wscf <> 'M'
027900051130     c                   z-add     clpscf        wksc
028000051130     c                   exsr      sr_spenota
028100051130     c                   if        (wfnota = *blanks and wfluo <> 'M') or
028200051130     c                              wfluo = 'I'
028300051130     c                   eval      wprt = 'S'
028400051130     c                   else
028500051130     c* se per scf ho trovato indirizzo e-mail me lo memorizzo per
028600051130     c* eventuali altri codici aventi lo stesso scf
028700051130     c                   eval      wscf = 'M'
028800051130     c                   endif
028900051130     c                   endif
029000051130     c                   endif
029100051130     c* se trovato luogo senza e-mail  --> no mail
029200051130     c                   when      wfluo = 'I'
029300051130     c                   eval      wprt = 'S'
029400051130    4c                   endsl
029500051130     c
029600051130     c* RICHIESTA STAMPA CLIENTI CON ANOMALIE INVIO FATTURA
029700051130     c                   when      d90anom = 'S' and wfnota ='S' and
029800051130     c                             wfluo <> *blanks
029900051130     c                   eval      wprt = 'S'
030000051130    3c                   endsl
030100051130    2c                   endif
030200051130     c* RAGGRUPPAMENTO DA STAMPARE
030300051130     c                   if        wprt = 'S'
030400060929     c* se richiesto ordinamento per sottoconto int.fattura stampo altriment
030500060929     c* se richiesto ordinamento per comm.le memorizzo in workfile
030600060929     c* sottoc.int.fatt./codice comm.le
030700060929     c                   if        v1cord='K'
030800060929     c* stampo  tutto il gruppo
030900051130     c                   exsr      sr_elabora
031000060929     c                   else
031100060929     c                   exsr      wrt_wf
031200060929     c                   endif
031300051130     c                   move      clpscf        w_clpscf
031400051130     c                   endif
031500051130     c                   endif
031600051130     C/EXEC SQL
031700051130     C+ Fetch listaclp into :cnclpxxds
031800051130     C/END-EXEC
031900051130
032000051130    1c                   EndDo
032100051130     c*
032200051130
032300051130     C/EXEC SQL
032400051130     C+ close listaclp
032500060929     C/END-EXEC
032600051130     c                   endsr
032700041118     c*
032800050513     c**********************************************************************
032900050513     c     Sr_Elabora    begsr
033000050513     c* stampo dati del sottoconto intestazione fattura
033100050513     C                   clear                   DS_cnaco
033200050513     C                   clear                   DS_cnind
033300050513     C                   clear                   DS_cnclp
033400050513     C                   clear                   DS_fncls
033500050513     c                   eval      I69kac = clpscf
033600050513     c                   eval      I69kcp = clpscf
033700050513     c                   eval      I69kcs = clpscf
033800050513     C                   CALL      'TIBS69R'
033900050513     C                   PARM                    tibs69DS
034000050513     C                   PARM                    DS_cnaco
034100050513     C                   PARM                    DS_cnind
034200050513     C                   PARM                    DS_cnclp
034300050513     C                   PARM                    DS_fncls
034400050513     c                   eval      punirsc = acorag
034500050513     c                   z-add     acoksc        pkscuni
034600050513     c                   clear                   pblk
034700050513     c                   if        acoabl = '8' or acoabl = '*'
034800050513     c                   eval      pblk = '*'
034900050513     c                   endif
035000050513     c                   eval      pfunif = %subst(clsflo:2:1)
035100050513     c**                 z-add     f_clptft      ptft
035200050513     c* decod. tipo fattura
035300050513     c                   Clear                   ptft_d
035400050513     c                   clear                   dsbs02
035500050513     C                   MOVEL     'C'           t02mod
035600050513     C                   MOVEL     knsif         t02sif
035700050513     C                   MOVEL     'TFT'         t02cod
035800050513     C                   MOVEL(P)  f_clptft      t02ke1
035900050513      *
036000050513     C                   CALL      'TIBS02R'
036100050513     C                   PARM                    KPJBA
036200050513     C                   PARM                    dsbs02
036300050513      *
036400050513     C                   if        t02err = *BLANKS
036500051129     c                   movel     t02uni        dtft
036600051129     c                   movel     §tftdes1      ptft_d
036700050513     c                   endif
036800050513
036900050513     c****               z-add     f_clpfft      pfft
037000050513     c* decod. frequenza fattura
037100050513     c                   Clear                   pfft_d
037200050513     c                   Eval      b = 1
037300050513     c     f_clpfft      Lookup    cff(b)                                 30
037400050513     c                   If        *In30
037500050513     c                   Movel     dff(b)        pfft_d
037600050513     c                   EndIf
037700050513     c*
037800050513     c                   movel     f_clpfun      ptdf              1
037900050513     c                   clear                   PTDF_D
038000050513     c* decod. tipo data fattura
038100050513     C                   MOVEL     '13'          TIPTAB
038200050513     C                   MOVEL(P)  ptdf          KEYTAB
038300050513     C     CHIAV4        CHAIN     TABEL00F                           91
038400050513     C     *IN91         IFEQ      *ON
038500050513     C     *IN91         OREQ      *OFF
038600050513     C     TBLFLG        ANDNE     ' '
038700050513     C                   ELSE
038800051129     C                   MOVEL     TBLUNI        ds13
038900051129     C                   MOVEL     §13des1       PTDF_D
039000050513     C                   ENDIF
039100051129     c* presenza luogo 001
039200051129     c* presenza nota 84
039300051129     c                   z-add     acoksc        wksc
039400051130     c                   exsr      sr_spenota
039500051129     c                   move      wfluo         pfluo
039600051129     c                   move      wfnota        pfnota
039700050513     c* sottoconto intestazione fattura se diverso
039800050513     c                   clear                   wprt006           1
039900050513     c                   if        f_clpscf <> f_clpksc
040000050513     c                   z-add     f_clpscf      PKSCUNI_U
040100050513     c* memorizzo di stampare rcd006
040200050513     c                   eval      wprt006 = 'S'
040300050513     c                   endif
040400060929     c* Gestione rottura cod.comm.le:
040500060929     c* se ordinamento per cod.comm.le, a rottura stampo riga del comm.le
040600060929     c                   if        v1cord='C' and clpage<>sav_age
040700060929     c                   exsr      sr_rotage
040800060929     c                   if        sav_age<>0
040900060929     c                   write     rcd007
041000061002     c                   write     rcd001
041100060929     c                   endif
041200060929     c                   write     rcdage
041300060929     c                   write     rcd003
041400060929     c                   z-add     9             $righe
041500060929     c                   z-add     clpage        sav_age
041600060929     c                   endif
041700060929     c*
041800050513     c                   if        ($righe + 1) > 60
041900050513     c                   write     rcd007
042000050513     c                   write     rcd001
042100060929     c                   if        v1cord='K'
042200050513     c                   write     rcd003
042300060929     c                   z-add     7             $righe
042400060929     c                   else
042500060929     c                   write     rcdage
042600060929     c                   write     rcd003
042700060929     c                   z-add     9             $righe
042800060929     c                   endif
042900050513     c                   endif
043000050513     c                   write     rcd004
043100050513     c                   add       1             $righe
043200050513     c
043300050513     c* stampo dati dei clienti unificati al sottoconto intest.fattura
043400050513     c
043500060929     c                   if        v1cord='C'
043600060929     c                   z-add     f_clpkcc      clpkcc
043700060929     c                   z-add     f_clpkut      clpkut
043800060929     c                   endif
043900050513     c                   clear                   wprtprima         1
044000050513     c     kclp          setll     cnclp02l
044100050513     c                   do        *hival
044200050513     c     kclp          reade     cnclp02l
044300050513     c                   if        %eof(cnclp02l)
044400050513     c                   leave
044500050513     c                   endif
044600050513     c* ignoro record se ksc = scf
044700050513     c                   if        F_clpscf = F_clpksc
044800050513     c                   iter
044900050513     c                   endif
045000050513     c*
045100050513     C                   clear                   DS_cnaco
045200050513     C                   clear                   DS_cnind
045300050513     C                   clear                   DS_fncls
045400050513     c                   eval      I69kac = f_clpksc
045500050513     c                   eval      I69kcs = f_clpksc
045600050513     C                   CALL      'TIBS69R'
045700050513     C                   PARM                    tibs69DS
045800050513     C                   PARM                    DS_cnaco
045900050513     C                   PARM                    DS_cnind
046000050513     C                   PARM                    ds_cnclp
046100050513     C                   PARM                    DS_fncls
046200050516     c* scarto se bloccato e richiesti solo i non bloccati
046300050516     c                   if        d90abl='E' and (acoabl='*' or acoabl='8')
046400050516     c                   iter
046500050516     c                   endif
046600050513     c                   eval      U_punirsc = acorag
046700050513     c                   z-add     acoksc        U_pkscuni
046800050513     c                   clear                   U_pblk
046900050513     c                   if        acoabl = '8' or acoabl = '*'
047000050513     c                   eval      U_pblk = '*'
047100050513     c                   endif
047200050513     c                   eval      U_pfunif = %subst(clsflo:2:1)
047300050513     c***                z-add     f_clptft      U_ptft
047400050513     c* decod. tipo fattura
047500050513     c                   Clear                   U_ptft_d
047600050513     c                   clear                   dsbs02
047700050513     C                   MOVEL     'C'           t02mod
047800050513     C                   MOVEL     knsif         t02sif
047900050513     C                   MOVEL     'TFT'         t02cod
048000050513     C                   MOVEL(P)  f_clptft      t02ke1
048100050513      *
048200050513     C                   CALL      'TIBS02R'
048300050513     C                   PARM                    KPJBA
048400050513     C                   PARM                    dsbs02
048500050513      *
048600050513     C                   if        t02err = *BLANKS
048700051129     c                   movel     t02uni        dtft
048800051129     c                   movel     §tftdes1      U_ptft_d
048900050513     c                   endif
049000050513
049100050513     c****               z-add     f_clpfft      U_pfft
049200050513     c* decod. frequenza fattura
049300050513     c                   Clear                   U_pfft_d
049400050513     c                   Eval      b = 1
049500050513     c     f_clpfft      Lookup    cff(b)                                 30
049600050513     c                   If        *In30
049700050513     c                   Movel     dff(b)        U_pfft_d
049800050513     c                   EndIf
049900050513     c*
050000050513     c                   movel     f_clpfun      U_ptdf            1
050100050513     c                   clear                   U_PTDF_D
050200050513     c* decod. tipo data fattura
050300050513     C                   MOVEL     '13'          TIPTAB
050400050513     C                   MOVEL(P)  U_ptdf        KEYTAB
050500050513     C     CHIAV4        CHAIN     TABEL00F                           91
050600050513     C     *IN91         IFEQ      *ON
050700050513     C     *IN91         OREQ      *OFF
050800050513     C     TBLFLG        ANDNE     ' '
050900050513     C                   ELSE
051000051129     C                   MOVEL     TBLUNI        ds13
051100051129     C                   MOVEL     §13des1       U_PTDF_D
051200050513     C                   ENDIF
051300051129     c* presenza luogo 001
051400051129     c* presenza nota 84
051500051129     c                   z-add     acoksc        wksc
051600051129     c                   exsr      sr_spenota
051700051129     c                   move      wfluo         U_pfluo
051800051129     c                   move      wfnota        U_pfnota
051900050513     c                   if        $righe > 60
052000050513     c                   write     rcd007
052100050513     c                   write     rcd001
052200060929     c                   if        v1cord='K'
052300050513     c                   write     rcd003
052400050516     C                   write     rcd004
052500060929     c                   z-add     8             $righe
052600060929     c                   else
052700060929     c                   write     rcdage
052800060929     c                   write     rcd003
052900060929     c                   write     rcd004
053000060929     c                   z-add     10            $righe
053100050513     c                   endif
053200060929     c                   endif
053300050513     c                   if        wprtprima = 'S' and  wprt006 = 'S'
053400050513     c                   eval      ppar = '('
053500050513     c                   eval      ppar1 = ')'
053600050513     c                   write     rcd006
053700050513     c                   clear                   wprt006
053800050513     c                   endif
053900050513     c                   write     rcd005
054000050513     c                   add       1             $righe
054100050513     c                   eval      wprtprima = 'S'
054200050513     c*
054300050513     c                   enddo
054400050513     c*
054500050513     c                   if        wprt006 = 'S'
054600050513     c                   eval      ppar = '('
054700050513     c                   eval      ppar1 = ')'
054800050513     c                   write     rcd006
054900050513     c                   write     rcdspace
055000050513     c                   add       1             $righe
055100050513     c                   endif
055200050513     c*
055300050513     c                   endsr
055400051129     c**********************************************************************
055500051129     c     sr_welab      begsr
055600051129     c                   clear                   w_elab            1
055700051129    4c****               if        d90abl = 'E'
055800051129     C                   clear                   DS_cnaco
055900051129     C                   clear                   DS_cnind
056000051129     C                   clear                   DS_cnclp
056100051129     C                   clear                   DS_fncls
056200051129     c                   eval      I69kac = clpksc
056300051129     C                   CALL      'TIBS69R'
056400051129     C                   PARM                    tibs69DS
056500051129     C                   PARM                    DS_cnaco
056600051129     C                   PARM                    DS_cnind
056700051129     C                   PARM                    DS_cnclp
056800051129     C                   PARM                    DS_fncls
056900051129    5c                   if        (acoabl = '8' or acoabl = '*') and
057000051129     c                             d90abl = 'E'
057100051129     c                   eval      w_elab = 'N'
057200051129    5c                   endif
057300051129    5c****               endif
057400051129     c                   endsr
057500051129     c**********************************************************************
057600051129     c     Sr_spenota    begsr
057700051129     c                   clear                   wfluo             1
057800051129     c                   clear                   wfnota            1
057900051129     c     kspe          chain     fnspe03l
058000051129      *
058100051129    1c                   if        %found(fnspe03l)
058200051129     c                   movel     'I'           wfluo
058300051129      * verifico se c'è indirizzo mail
058400051129     c     ksp2          chain     fnsp201l
058500051129    2c                   if        %found(fnsp201l) and SP2EST <> *BLANKS
058600051129     c                   movel     'M'           wfluo
058700051129    2c                   endif
058800051129    1c                   endif
058900051129
059000051129     c                   eval      Knk1 = '0151' + %editc(wksc:'X')
059100051129     c     Kntc          chain     Tfntc01l
059200051129    1c                   If        %found(tfntc01l) and ntcrnt <> *blanks
059300051129     c                   eval      wfnota = 'S'
059400051129    1c                   endif
059500051129     c                   endsr
059600060929      *---------------------------------------------------------------*
059700060929     C* Scrittura file di work per stampa ordinata per comm.le
059800060929      *---------------------------------------------------------------*
059900060929     C     wrt_wf        BEGSR
060000060929     C                   clear                   DS_cnaco
060100060929     C                   clear                   DS_cnind
060200060929     C                   clear                   DS_cnclp
060300060929     C                   clear                   DS_fncls
060400060929     c                   eval      I69kcp = clpscf
060500060929     C                   CALL      'TIBS69R'
060600060929     C                   PARM                    tibs69DS
060700060929     C                   PARM                    DS_cnaco
060800060929     C                   PARM                    DS_cnind
060900060929     C                   PARM                    DS_cnclp
061000060929     C                   PARM                    DS_fncls
061100060929     c                   clear                   wfta9100
061200060929     c                   z-add     clpscf        wta91scf
061300061002     c* recupero il commerciale unificante
061400061002     C                   MOVEL     '01'          TIPTAB
061500061002     C                   MOVEL(P)  f_clpage      KEYTAB
061600061002     C     CHIAV4        CHAIN     TABEL00F
061700061002     C                   MOVEL     TBLUNI        ds01
061800061002     C                   MOVEL     §01rgf        wta91age
061900060929     c                   write     wfta9100
062000060929     c                   endsr
062100060929      *---------------------------------------------------------------*
062200060929     C* Operazioni a rotuttra di cod.comm.le
062300060929      *---------------------------------------------------------------*
062400060929     C     sr_rotage     BEGSR
062500060929     c                   clear                   pdesage
062600060929     c* reperimento decodifica comm.le da tab.01
062700060929     C                   MOVEL     '01'          TIPTAB
062800060929     C                   MOVEL(P)  clpage        KEYTAB
062900060929     C     CHIAV4        CHAIN     TABEL00F                           91
063000060929     C     *IN91         IFEQ      *ON
063100060929     C     *IN91         OREQ      *OFF
063200060929     C     TBLFLG        ANDNE     ' '
063300060929     C                   ELSE
063400060929     C                   MOVEL     TBLUNI        ds01
063500060929     C                   MOVEL     §01age        Pdesage
063600060929     C                   ENDIF
063700060929     c                   endsr
063800041124      *---------------------------------------------------------------*
063900970224     C* OPERAZIONI INIZIALI
064000041124      *---------------------------------------------------------------*
064100970224     C     *INZSR        BEGSR
064200970224     C*
064300970224     C     *ENTRY        PLIST
064400970224     C                   PARM                    KPJBA
064500050516     C                   MOVEl     KPJBU         tnta90ds
064600970224     C*
064700041116     c     *dtaara       define    §azute        azuteds
064800041116     c     *dtaara       define    §datiute      ddatiute
064900041116     c                   in(E)     *dtaara
065000041116     c                   If        %error  or RSUT = *blanks
065100041116     c                   CLEAR                   tibs34ds
065200041116     c                   CALL      'TIBS34R'
065300041116     c                   PARM                    tibs34ds
065400041116     c                   in        *dtaara
065500041116     c                   EndIf
065600050516     c* carico schiera pog dei p.o. da elaborare
065700050516     c                   clear                   pog
065800050516     c                   if        v1cpop <> '999'
065900050516     c                   movel     v1cpop        pog(1)
066000050516     c                   else
066100050516     c                   Clear                   wabi
066200050516      * Verifica errori e autorità profilo
066300050516s   1c                   Select
066400050516      * se ho errori nei dati utente esco dal pgm
066500050516      * se non c'è l'abilitazione
066600050516      * --> se 1° livello, abilitazioni al terminal
066700050516      *     se 2° livello, abilitazioni al punto operativo
066800050516w   1c                   When      UteAut = *Blanks
066900050516if  2c                   If        DutLpo = '1'
067000050516     c                   Eval      wabi   = 'TP'
067100050516e   2c                   EndIf
067200050516if  2c                   If        DutLpo = '2'
067300050516     c                   Eval      wabi   = 'PO'
067400050516e   2c                   EndIf
067500050516if  2c                   If        DutLpo = 'S'
067600050516     c                   Eval      wabi   = 'AZ'
067700050516e   2c                   EndIf
067800050516      * carica le abilitazioni del profilo
067900050516x   1c                   Other
068000050516     c                   Movel     UteFaf        Dute01
068100050516     c                   If        §UteCli <> *Blanks
068200050516     c                   Eval      wabi = §UteCli
068300050516     c                   Else
068400050516     c                   Eval      wabi = UteAut
068500050516     c                   EndIf
068600050516e   1c                   EndSl
068700050516
068800050516      * Reperimento dei P.O. gestibili dall'utente
068900050516     c                   clear                   TRUL31DS
069000050516     c                   eval      I31abi = wabi
069100050516     c                   eval      I31cdi = DUTdis
069200050516     c                   eval      I31car = DUTare
069300050516     c                   eval      I31cpo = DUTpou
069400050516     c                   call      'TRUL31R'
069500050516     c                   parm                    KPJBA
069600050516     c                   parm                    TRUL31DS
069700050516if  1c                   if        O31pog > *zeros
069800050516     c                   movea     O31pog        pog
069900050516e   1c                   endif
070000050516     c                   endif
070100050516     c
070200041118     c*
070300970224     C                   TIME                    WTIME            14 0
070400970224     C                   MOVE      WTIME         WDATE             8 0
070500970311     C                   MOVEL     WTIME         UTIME             6 0
070600050513     c*
070700050513     c     kclp          klist
070800050513     c                   kfld                    clpkut
070900050513     c                   kfld                    clpkcc
071000050513     c                   kfld                    clpscf
071100050513     C*-----CHIAVE FILE     TABEL00F
071200050513     C     CHIAV4        KLIST
071300050513     C                   KFLD                    codut
071400050513     C                   KFLD                    TIPTAB            2
071500050513     C                   KFLD                    KEYTAB
071600050513     C*-----CHIAVE FILE     TABEL00F
071700050513     C     KTA§          KLIST
071800050513     C                   KFLD                    codut
071900050513     C                   KFLD                    TIPTAB
072000051129      ** ACCESSO ANAGRAFICHE CLIENTI
072100051129     C     KSPE          KLIST
072200051129     C                   KFLD                    kFLS
072300051129     C                   KFLD                    wksc
072400051129     C                   KFLD                    kcod
072500051129      * accesso al file luoghi con indirizzo mail
072600051129     c     Ksp2          Klist
072700051129     c                   kfld                    specli
072800051129     c                   kfld                    specod
072900051129     c                   kfld                    comtpe
073000051129      *  file note tfntc01l
073100051129     C     Kntc          KLIST
073200051129     C                   KFLD                    Kapl
073300051129     C                   KFLD                    Knk1
073400051129     C                   KFLD                    Knk2
073500051129     C                   KFLD                    Ktnt
073600050513     C***
073700050513     C* CARICO TABELLA    FF  - FREQUENZA FATTURA
073800050513     C***
073900050513     C                   MOVEL     'FF'          TIPTAB
074000050513     C                   Z-ADD     0             B                 2 0
074100050513     C     KTA§          CHAIN     TABEL                              71
074200050513     C     *IN71         DOWEQ     *OFF
074300050513     C     TBLFLG        IFEQ      ' '
074400050513     C                   ADD       1             B
074500050513     C                   MOVEL     TBLKEY        CFF(B)
074600050513     C                   MOVEL(p)  TBLUNI        dsff
074700051129     c                   Movel     §ffdes1       DFF(B)
074800050513     C                   ENDIF
074900050513     C     KTA§          READE     TABEL                                  71
075000050513     C                   ENDDO
075100061002     c                   if        v1cord='C'
075200061002     c                   open      wfta911l
075300061002     c                   endif
075400051130     c* titolo dello spool
075500051130     c                   select
075600051130     c                   when      d90fatt = 'S'
075700051130     c                   eval      ptitolo = titfatt
075800051130     c                   when      d90anom = 'S'
075900051130     c                   eval      ptitolo = titanoma
076000051130     c                   other
076100051130     c                   eval      ptitolo = titolo
076200051130     c                   endsl
076300051201     c                   if        v1cpop <> '999'
076400051201     c                   movel(p)  v1cpop        p1ccli
076500051201     c                   else
076600051201     c                   movel     'Tutti'       p1ccli
076700051201     c                   endif
076800051201     c
076900050513     c*
077000050513     c                   write     rcd001
077100060929     c                   if        v1cord='K'
077200050513     c                   write     rcd003
077300060929     c                   z-add     7             $righe
077400060929     c                   else
077500060929     c                   z-add     3             $righe
077600060929     c                   endif
077700970224     C                   ENDSR
