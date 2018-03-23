000100041123     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200041124
000300050512     H* TNTA91R *-----------------------------------------------------*
000400050512     H*         - Elenco clienti per sottoconto intestazione fattura  *
000500970225     H*---------------------------------------------------------------*
000600050513     FCNCLP02l  IF   E           K DISK    prefix(F_)
000700050513     Ftabel00f  IF   E           K DISK
000800130905     fAZCMM01L  if   e           k disk
000900051129     Ffnspe03l  if   e           k disk
001000051129     Ffnsp201l  if   e           k disk
001100051129     Ftfntc01l  if   e           k disk
001200061002     Fwfta911l  uf a e           k disk    usropn
001300050513     FTNTA91P   O    E             PRINTER oflind(*In99)
001400041116
001500050513     D CFF             S              1  0 DIM(15)
001600050513     D DFF             S             30    DIM(15)
001700050516     d pog             s              3    dim(250)
001800050516     d wabi            s                   like(UteAut)
001900050513     d dsff          e ds
002000051129     d dtft          e ds
002100051129     d ds13          e ds
002200050516     d tnta90ds        ds
002300050516     d  v1cpop                 1      3
002400050516     d  d90abl                 4      4
002500051130     d  d90fatt                5      5
002600051130     d  d90anom                6      6
002700060929     d* ordinamento K=cliente; C=commerciale
002800060929     d  v1cord                 7      7
002900970224     D KPJBA         E DS
003000041116     d Azuteds       e ds                  Extname(Azute00f)
003100041116     d dDatiute      e ds
003200041116     d Tibs34ds      e ds                  Inz
003300050516     d dute01        e ds
003400050516     d trul31ds      e ds
003500050513     D* DS PER TIBS02R - GESTIONE TNTBE00F
003600050513     D DSBS02        E DS                  EXTNAME(TIBS02DS)
003700050513     d cnclpxxDs     e ds                  ExtName(cnclp00f)
003800050513     d TIBS69DS      E DS                  INZ
003900050513     d DS_cnaco      E DS                  extname(CNACO00F)
004000050513     d DS_cnind      E DS                  extname(CNIND00F)
004100050513     d DS_cnclp      E DS                  extname(CNCLP00F)
004200050513     d                                     prefix(F_)
004300050513     d DS_fncls      E DS                  extname(FNCLS00F)
004400050513     d
004500051201     D WrkGetListaclp  S           4500
004600050513     D                                     VARYING
004700051201     D WrkclpFilIn     S           4000
004800050513     D                                     VARYING
004900050513     d i               S              5I 0
005000041124
005100050512     D codut           S              1  0 inz(1)
005200050513     D keytab          S                   like(tblkey)
005300050513     d w_clpscf        s                   Like(clpscf)
005400051130     d s_clpscf        s                   Like(clpscf)
005500051129     d wksc            s                   Like(acoksc)
005600130905     d sav_age         s                   Like(clpage) inz(*hival)
005700050513     d $righe          s              2  0
005800051129     D Kapl            s                   LIKE(NTCapl) inz('C')
005900051129     D Knk1            s                   LIKE(NTCnk1)
006000051129     D Knk2            s                   LIKE(NTCnk2)
006100051129     D Ktnt            s                   LIKE(NTCtnt) inz('84')
006200051129     D Kfls            s                   LIKE(spefls) inz('L')
006300051129     D Kcod            s                   LIKE(specod) inz('001')
006400051129     D comtpe          s                   like(sp2tpe) inz('EM')
006500051130     D TITfatt         C                   CONST('         ELENCO CLIENTI PER S-
006600051130     D                                     OTTOCONTO INTESTAZIONE FATTURA SENZA-
006700051130     D                                      INVIO FATTURA VIA E-MAIL         ')
006800051130     D TITanoma        C                   CONST('           ELENCO CLIENTI PER-
006900051201     D                                      SOTTOCONTO INTESTAZIONE FATTURA CON-
007000051130     D                                      ANOMALIE INVIO FATTURA           ')
007100051130     D TITolo          C                   CONST('E L E N C O   C L I E N T I  -
007200051130     D                                      P E R   S O T T O C O N T O   I N T-
007300051130     D                                      E S T A Z I O N E   F A T T U R A')
007400041124
007500970228      *---------------------------------------------------------------*
007600130905     C/EXEC SQL
007700130905     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007800130905     C/END-EXEC
007900130905     C*
008000050512     c* Eseguo sql per estrarre da cnclp solo i dati che mi servono
008100050512     c                   exsr      sr_Sqlclp
008200050512     c* Lettura ed elaborazione di cnclp
008300051130     c                   if        d90fatt <> 'S' and d90anom <> 'S'
008400050513     c                   exsr      sr_Leggiclp
008500051130     c                   else
008600051130     c                   exsr      sr_Leggiclp1
008700051130     c                   endif
008800060929     c* se richiesto ordinamento per codice comm.le stampo leggendo il file
008900060929     c* di work
009000060929     c                   if        v1cord='C'
009100130905     c                   reset                   sav_age
009200060929     c     *loval        setll     wfta911l
009300060929     c                   do        *hival
009400060929     c                   read      wfta911l
009500060929     c                   if        %eof(wfta911l)
009600060929     c                   leave
009700060929     c                   endif
009800060929     c                   z-add     wta91scf      clpscf
009900060929     c                   z-add     wta91age      clpage
010000060929     c                   exsr      sr_elabora
010100060929     c                   enddo
010200060929     c                   endif
010300050512     c
010400050512     c                   seton                                        lr
010500050513     c                   write     fines
010600051130     c                   write     rcd007
010700050513     c*
010800050513     C                   eval      I69TLA  = 'C'
010900050513     C                   call      'TIBS69R'
011000050513     C                   parm                    TIBS69DS
011100050512     c**********************************************************************
011200050512     c     Sr_Sqlclp     begsr
011300050513     **?Imposto la parte fissa dell'istruzione.
011400050513     C                   EVAL      WrkGetListaCLP =
011500050513     C                             'SELECT * from cnclp00f'
011600050513     **?WHERE
011700050513     C                   EVAL      WrkGetListaCLP = WrkGetListaCLP +
011800050516     C                             ' WHERE clpscf > 0 '
011900050513     **?Selezione dei codici appartenenti ai po richiesti
012000050513     C                   EXSR      WhrPOArea
012100050513     **?Selezione su codice utente e capoconto
012200050513     C                   EVAL      WrkGetListaCLP = WrkGetListaCLP
012300050513     C                             +
012400050513     C                             ' AND clpkut = ' + %triml(%editc(codut:'Z'))
012500050513     c                             +
012600050513     c                             ' and clpkcc = ' + %triml(%editc(dutkci:'Z'))
012700050513     C                   EVAL      WrkGetListaCLP = WrkGetListaCLP
012800050513     C                             + ' ORDER BY clpkut, clpkcc, clpscf '
012900050513     **?Solo lettura.
013000050513     C                   EVAL      WrkGetListaCLP = WrkGetListaCLP +
013100050513     C                             ' FOR READ ONLY'
013200050513     C/EXEC SQL
013300050513     C+ PREPARE ListaCLP FROM :WrkGetListaCLP
013400050513     C/END-EXEC
013500050513     C
013600050513     C/EXEC SQL
013700050513     C+ DECLARE ListaCLP CURSOR FOR ListaCLP
013800050513     C/END-EXEC
013900050513     C
014000050513     C/EXEC SQL
014100050513     C+ OPEN ListaCLP
014200050513     C/END-EXEC
014300050512     c                   endsr
014400050513     ***********************************************************************
014500050513     **?
014600050513     **?Stringa WHERE con i PO richiesti
014700050513     **?
014800050513     ***********************************************************************
014900050513     C     WhrPOArea     BEGSR
015000050513     C                   RESET                   WrkClpFilIn
015100050513     **?Reperisco i PO che compongono l'area amministrativa.
015200050513
015300050513    1C                   DO        250           i
015400050513
015500050513    2C                   IF        POG(i) = '000' or pog(i) = '   '
015600050513     C
015700050513     C                   LEAVE
015800050513     C
015900050513   x2C                   ELSE
016000050513     **?L'utente è autorizzato al PO, compongo la stringa IN.
016100050513    3C                   SELECT
016200050513     C                   WHEN      %LEN(WrkClpFilIn) = 0
016300050513     C                   EVAL      WrkClpFilIn = ' AND (substr(digits(clpscf),-
016400050513     C                              1, 3) in (''' + pog(i) + ''''
016500050513     C                   WHEN      %LEN(WrkClpFilIn) > 0
016600050513     C                   EVAL      WrkClpFilIn = WrkClpFilIn
016700050513     C                             + ' , ''' +  pog(i) + ''''
016800050513    3C                   ENDSL
016900050513     C
017000050513    2C                   ENDIF
017100050513
017200050513    1C                   ENDDO
017300050513     c*  Aggiungo parentesi a fine istruzione IN
017400050513     C                   EVAL      WrkClpFilIn = WrkClpFilIn + ')'
017500050513
017600050513    1C                   DO        250           i
017700050513
017800050513    2C                   IF        POG(i) = '000' or pog(i) = '   '
017900050513     C
018000050513     C                   LEAVE
018100050513     C
018200050513   x2C                   ELSE
018300050513     **?L'utente è autorizzato al PO, compongo la stringa IN.
018400050513    3C                   SELECT
018500050513     C                   WHEN      i = 1
018600050513     C                   EVAL      WrkClpFilIn = wrkClpFilIn
018700050513     C                             + ' OR substr(digits(clpksc), 1, 3) in('''
018800050513     C                             + pog(i) + ''''
018900050513     C                   WHEN      %LEN(WrkClpFilIn) > 1
019000050513     C                   EVAL      WrkClpFilIn = WrkClpFilIn
019100050513     C                             + ' , ''' + pog(i) + ''''
019200050513    3C                   ENDSL
019300050513     C
019400050513    2C                   ENDIF
019500050513
019600050513    1C                   ENDDO
019700050513     c*
019800050513     c* aggiungo )) a fine istruzione in
019900050513     C                   EVAL      WrkClpFilIn = WrkClpFilIn + '))'
020000050513
020100050513     C                   EVAL      WrkGetListaClp = WrkGetListaClp
020200050513     C                             + WrkClpFilIn
020300050513     C                   ENDSR
020400050512     c**********************************************************************
020500050513     c     Sr_LeggiClp   begsr
020600050513     c                   clear                   w_clpscf
020700050512     c*
020800050513     C/EXEC SQL
020900050513     C+ Fetch listaclp into :cnclpxxds
021000050513     C/END-EXEC
021100051129    1c                   Dow       Sqlcod = 0
021200050513     c* a rottura di sottoconto intestazione fattura rileggo cnclp per
021300050513     c* stampare tutti i clienti appartenenti allo stesso gruppo
021400050513
021500050513     c* scarto record se scf=ksc perchè devo andare in stampa solo se presen
021600050513     c* te più di un cliente
021700051129    2c                   if        clpscf <> clpksc
021800050516     c* se richiesti gli unificati non bloccati scarto record se
021900050516     c* unificato bloccato
022000051129    3c                   if        clpscf <> w_clpscf
022100051129     c                   exsr      sr_welab
022200051129    4c                   if        w_elab = ' '
022300060929     c* se richiesto ordinamento per sottoconto int.fattura stampo altriment
022400060929     c* se richiesto ordinamento per comm.le memorizzo in workfile
022500060929     c* sottoc.int.fatt./codice comm.le
022600060929     c                   if        v1cord='K'
022700050513     c                   exsr      sr_elabora
022800060929     c                   else
022900060929     c                   exsr      wrt_wf
023000060929     c                   endif
023100050513     c                   eval      w_clpscf = clpscf
023200051129    4c                   endif
023300051129    3c                   endif
023400051129    2c                   endif
023500050513     C/EXEC SQL
023600050513     C+ Fetch listaclp into :cnclpxxds
023700050513     C/END-EXEC
023800050513
023900051129    1c                   EndDo
024000050513     c*
024100050512
024200050513     C/EXEC SQL
024300050513     C+ close listaclp
024400050513     C/END-EXEC
024500050512     c                   endsr
024600051130     c**********************************************************************
024700051130     c     Sr_leggiclp1  begsr
024800051130     c                   clear                   w_clpscf
024900051130     c                   clear                   s_clpscf
025000051130     c*
025100051130     C/EXEC SQL
025200051130     C+ Fetch listaclp into :cnclpxxds
025300051130     C/END-EXEC
025400051130    1c                   Dow       Sqlcod = 0
025500051130     c                   if        clpscf <> s_clpscf
025600051130     c                   clear                   wscf              1
025700051130     c                   z-add     clpscf        s_clpscf
025800051130     c                   endif
025900051130     c                   if        clpscf <> w_clpscf
026000051130     c* se richiesti gli unificati non bloccati scarto record se
026100051130     c* unificato bloccato
026200051130     c                   exsr      sr_welab
026300051130     c                   clear                   wprt              1
026400051130    2c                   if        w_elab = ' '
026500051130     c* letto codice non bloccato verifico in base alle selezioni se
026600051130     c* cliente da stampare
026700051130     c* verifico se raggruppamento da stampare
026800051130     c                   z-add     acoksc        wksc
026900051130     c                   exsr      sr_spenota
027000051130    3c                   select
027100051130     c* RICHIESTA STAMPA CLIENTI NO E-MAIL
027200051130     c                   when      d90fatt = 'S'
027300051130    4c                   select
027400051130     c* se non tovato ne' luogo ne' nota verifico il sottoconto intestaz.
027500051130     c* fattura
027600051130     c                   when      wfnota = *blanks  and wfluo=*blanks
027700051130     c                   if        clpscf = wksc
027800051130     c                   eval      wprt  = 'S'
027900051130     c                   else
028000051130     c                   if        wscf <> 'M'
028100051130     c                   z-add     clpscf        wksc
028200051130     c                   exsr      sr_spenota
028300051130     c                   if        (wfnota = *blanks and wfluo <> 'M') or
028400051130     c                              wfluo = 'I'
028500051130     c                   eval      wprt = 'S'
028600051130     c                   else
028700051130     c* se per scf ho trovato indirizzo e-mail me lo memorizzo per
028800051130     c* eventuali altri codici aventi lo stesso scf
028900051130     c                   eval      wscf = 'M'
029000051130     c                   endif
029100051130     c                   endif
029200051130     c                   endif
029300051130     c* se trovato luogo senza e-mail  --> no mail
029400051130     c                   when      wfluo = 'I'
029500051130     c                   eval      wprt = 'S'
029600051130    4c                   endsl
029700051130     c
029800051130     c* RICHIESTA STAMPA CLIENTI CON ANOMALIE INVIO FATTURA
029900051130     c                   when      d90anom = 'S' and wfnota ='S' and
030000051130     c                             wfluo <> *blanks
030100051130     c                   eval      wprt = 'S'
030200051130    3c                   endsl
030300051130    2c                   endif
030400051130     c* RAGGRUPPAMENTO DA STAMPARE
030500051130     c                   if        wprt = 'S'
030600060929     c* se richiesto ordinamento per sottoconto int.fattura stampo altriment
030700060929     c* se richiesto ordinamento per comm.le memorizzo in workfile
030800060929     c* sottoc.int.fatt./codice comm.le
030900060929     c                   if        v1cord='K'
031000060929     c* stampo  tutto il gruppo
031100051130     c                   exsr      sr_elabora
031200060929     c                   else
031300060929     c                   exsr      wrt_wf
031400060929     c                   endif
031500051130     c                   move      clpscf        w_clpscf
031600051130     c                   endif
031700051130     c                   endif
031800051130     C/EXEC SQL
031900051130     C+ Fetch listaclp into :cnclpxxds
032000051130     C/END-EXEC
032100051130
032200051130    1c                   EndDo
032300051130     c*
032400051130
032500051130     C/EXEC SQL
032600051130     C+ close listaclp
032700060929     C/END-EXEC
032800051130     c                   endsr
032900041118     c*
033000050513     c**********************************************************************
033100050513     c     Sr_Elabora    begsr
033200050513     c* stampo dati del sottoconto intestazione fattura
033300050513     C                   clear                   DS_cnaco
033400050513     C                   clear                   DS_cnind
033500050513     C                   clear                   DS_cnclp
033600050513     C                   clear                   DS_fncls
033700050513     c                   eval      I69kac = clpscf
033800050513     c                   eval      I69kcp = clpscf
033900050513     c                   eval      I69kcs = clpscf
034000050513     C                   CALL      'TIBS69R'
034100050513     C                   PARM                    tibs69DS
034200050513     C                   PARM                    DS_cnaco
034300050513     C                   PARM                    DS_cnind
034400050513     C                   PARM                    DS_cnclp
034500050513     C                   PARM                    DS_fncls
034600050513     c                   eval      punirsc = acorag
034700050513     c                   z-add     acoksc        pkscuni
034800050513     c                   clear                   pblk
034900130315     c                   if        acoabl <> *blanks
035000050513     c                   eval      pblk = '*'
035100050513     c                   endif
035200050513     c                   eval      pfunif = %subst(clsflo:2:1)
035300050513     c**                 z-add     f_clptft      ptft
035400050513     c* decod. tipo fattura
035500050513     c                   Clear                   ptft_d
035600050513     c                   clear                   dsbs02
035700050513     C                   MOVEL     'C'           t02mod
035800050513     C                   MOVEL     knsif         t02sif
035900050513     C                   MOVEL     'TFT'         t02cod
036000050513     C                   MOVEL(P)  f_clptft      t02ke1
036100050513      *
036200050513     C                   CALL      'TIBS02R'
036300050513     C                   PARM                    KPJBA
036400050513     C                   PARM                    dsbs02
036500050513      *
036600050513     C                   if        t02err = *BLANKS
036700051129     c                   movel     t02uni        dtft
036800051129     c                   movel     §tftdes1      ptft_d
036900050513     c                   endif
037000050513
037100050513     c****               z-add     f_clpfft      pfft
037200050513     c* decod. frequenza fattura
037300050513     c                   Clear                   pfft_d
037400050513     c                   Eval      b = 1
037500050513     c     f_clpfft      Lookup    cff(b)                                 30
037600050513     c                   If        *In30
037700050513     c                   Movel     dff(b)        pfft_d
037800050513     c                   EndIf
037900050513     c*
038000050513     c                   movel     f_clpfun      ptdf              1
038100050513     c                   clear                   PTDF_D
038200050513     c* decod. tipo data fattura
038300050513     C                   MOVEL     '13'          TIPTAB
038400050513     C                   MOVEL(P)  ptdf          KEYTAB
038500050513     C     CHIAV4        CHAIN     TABEL00F                           91
038600050513     C     *IN91         IFEQ      *ON
038700050513     C     *IN91         OREQ      *OFF
038800050513     C     TBLFLG        ANDNE     ' '
038900050513     C                   ELSE
039000051129     C                   MOVEL     TBLUNI        ds13
039100051129     C                   MOVEL     §13des1       PTDF_D
039200050513     C                   ENDIF
039300051129     c* presenza luogo 001
039400051129     c* presenza nota 84
039500051129     c                   z-add     acoksc        wksc
039600051130     c                   exsr      sr_spenota
039700051129     c                   move      wfluo         pfluo
039800051129     c                   move      wfnota        pfnota
039900050513     c* sottoconto intestazione fattura se diverso
040000050513     c                   clear                   wprt006           1
040100050513     c                   if        f_clpscf <> f_clpksc
040200050513     c                   z-add     f_clpscf      PKSCUNI_U
040300050513     c* memorizzo di stampare rcd006
040400050513     c                   eval      wprt006 = 'S'
040500050513     c                   endif
040600060929     c* Gestione rottura cod.comm.le:
040700060929     c* se ordinamento per cod.comm.le, a rottura stampo riga del comm.le
040800060929     c                   if        v1cord='C' and clpage<>sav_age
040900060929     c                   exsr      sr_rotage
041000130905     c                   if        sav_age <> *hival
041100060929     c                   write     rcd007
041200061002     c                   write     rcd001
041300060929     c                   endif
041400060929     c                   write     rcdage
041500060929     c                   write     rcd003
041600060929     c                   z-add     9             $righe
041700060929     c                   z-add     clpage        sav_age
041800060929     c                   endif
041900060929     c*
042000050513     c                   if        ($righe + 1) > 60
042100050513     c                   write     rcd007
042200050513     c                   write     rcd001
042300060929     c                   if        v1cord='K'
042400050513     c                   write     rcd003
042500060929     c                   z-add     7             $righe
042600060929     c                   else
042700060929     c                   write     rcdage
042800060929     c                   write     rcd003
042900060929     c                   z-add     9             $righe
043000060929     c                   endif
043100050513     c                   endif
043200050513     c                   write     rcd004
043300050513     c                   add       1             $righe
043400050513     c
043500050513     c* stampo dati dei clienti unificati al sottoconto intest.fattura
043600050513     c
043700060929     c                   if        v1cord='C'
043800060929     c                   z-add     f_clpkcc      clpkcc
043900060929     c                   z-add     f_clpkut      clpkut
044000060929     c                   endif
044100050513     c                   clear                   wprtprima         1
044200050513     c     kclp          setll     cnclp02l
044300050513     c                   do        *hival
044400050513     c     kclp          reade     cnclp02l
044500050513     c                   if        %eof(cnclp02l)
044600050513     c                   leave
044700050513     c                   endif
044800050513     c* ignoro record se ksc = scf
044900050513     c                   if        F_clpscf = F_clpksc
045000050513     c                   iter
045100050513     c                   endif
045200050513     c*
045300050513     C                   clear                   DS_cnaco
045400050513     C                   clear                   DS_cnind
045500050513     C                   clear                   DS_fncls
045600050513     c                   eval      I69kac = f_clpksc
045700050513     c                   eval      I69kcs = f_clpksc
045800050513     C                   CALL      'TIBS69R'
045900050513     C                   PARM                    tibs69DS
046000050513     C                   PARM                    DS_cnaco
046100050513     C                   PARM                    DS_cnind
046200050513     C                   PARM                    ds_cnclp
046300050513     C                   PARM                    DS_fncls
046400050516     c* scarto se bloccato e richiesti solo i non bloccati
046500130315     c                   if        d90abl='E' and acoabl<> *blanks
046600050516     c                   iter
046700050516     c                   endif
046800050513     c                   eval      U_punirsc = acorag
046900050513     c                   z-add     acoksc        U_pkscuni
047000050513     c                   clear                   U_pblk
047100130315     c                   if        acoabl <> *blanks
047200050513     c                   eval      U_pblk = '*'
047300050513     c                   endif
047400050513     c                   eval      U_pfunif = %subst(clsflo:2:1)
047500050513     c***                z-add     f_clptft      U_ptft
047600050513     c* decod. tipo fattura
047700050513     c                   Clear                   U_ptft_d
047800050513     c                   clear                   dsbs02
047900050513     C                   MOVEL     'C'           t02mod
048000050513     C                   MOVEL     knsif         t02sif
048100050513     C                   MOVEL     'TFT'         t02cod
048200050513     C                   MOVEL(P)  f_clptft      t02ke1
048300050513      *
048400050513     C                   CALL      'TIBS02R'
048500050513     C                   PARM                    KPJBA
048600050513     C                   PARM                    dsbs02
048700050513      *
048800050513     C                   if        t02err = *BLANKS
048900051129     c                   movel     t02uni        dtft
049000051129     c                   movel     §tftdes1      U_ptft_d
049100050513     c                   endif
049200050513
049300050513     c****               z-add     f_clpfft      U_pfft
049400050513     c* decod. frequenza fattura
049500050513     c                   Clear                   U_pfft_d
049600050513     c                   Eval      b = 1
049700050513     c     f_clpfft      Lookup    cff(b)                                 30
049800050513     c                   If        *In30
049900050513     c                   Movel     dff(b)        U_pfft_d
050000050513     c                   EndIf
050100050513     c*
050200050513     c                   movel     f_clpfun      U_ptdf            1
050300050513     c                   clear                   U_PTDF_D
050400050513     c* decod. tipo data fattura
050500050513     C                   MOVEL     '13'          TIPTAB
050600050513     C                   MOVEL(P)  U_ptdf        KEYTAB
050700050513     C     CHIAV4        CHAIN     TABEL00F                           91
050800050513     C     *IN91         IFEQ      *ON
050900050513     C     *IN91         OREQ      *OFF
051000050513     C     TBLFLG        ANDNE     ' '
051100050513     C                   ELSE
051200051129     C                   MOVEL     TBLUNI        ds13
051300051129     C                   MOVEL     §13des1       U_PTDF_D
051400050513     C                   ENDIF
051500051129     c* presenza luogo 001
051600051129     c* presenza nota 84
051700051129     c                   z-add     acoksc        wksc
051800051129     c                   exsr      sr_spenota
051900051129     c                   move      wfluo         U_pfluo
052000051129     c                   move      wfnota        U_pfnota
052100050513     c                   if        $righe > 60
052200050513     c                   write     rcd007
052300050513     c                   write     rcd001
052400060929     c                   if        v1cord='K'
052500050513     c                   write     rcd003
052600050516     C                   write     rcd004
052700060929     c                   z-add     8             $righe
052800060929     c                   else
052900060929     c                   write     rcdage
053000060929     c                   write     rcd003
053100060929     c                   write     rcd004
053200060929     c                   z-add     10            $righe
053300050513     c                   endif
053400060929     c                   endif
053500050513     c                   if        wprtprima = 'S' and  wprt006 = 'S'
053600050513     c                   eval      ppar = '('
053700050513     c                   eval      ppar1 = ')'
053800050513     c                   write     rcd006
053900050513     c                   clear                   wprt006
054000050513     c                   endif
054100050513     c                   write     rcd005
054200050513     c                   add       1             $righe
054300050513     c                   eval      wprtprima = 'S'
054400050513     c*
054500050513     c                   enddo
054600050513     c*
054700050513     c                   if        wprt006 = 'S'
054800050513     c                   eval      ppar = '('
054900050513     c                   eval      ppar1 = ')'
055000050513     c                   write     rcd006
055100050513     c                   write     rcdspace
055200050513     c                   add       1             $righe
055300050513     c                   endif
055400050513     c*
055500050513     c                   endsr
055600051129     c**********************************************************************
055700051129     c     sr_welab      begsr
055800051129     c                   clear                   w_elab            1
055900051129    4c****               if        d90abl = 'E'
056000051129     C                   clear                   DS_cnaco
056100051129     C                   clear                   DS_cnind
056200051129     C                   clear                   DS_cnclp
056300051129     C                   clear                   DS_fncls
056400051129     c                   eval      I69kac = clpksc
056500051129     C                   CALL      'TIBS69R'
056600051129     C                   PARM                    tibs69DS
056700051129     C                   PARM                    DS_cnaco
056800051129     C                   PARM                    DS_cnind
056900051129     C                   PARM                    DS_cnclp
057000051129     C                   PARM                    DS_fncls
057100130315    5c                   if        acoabl <> *blanks and
057200051129     c                             d90abl = 'E'
057300051129     c                   eval      w_elab = 'N'
057400051129    5c                   endif
057500051129    5c****               endif
057600051129     c                   endsr
057700051129     c**********************************************************************
057800051129     c     Sr_spenota    begsr
057900051129     c                   clear                   wfluo             1
058000051129     c                   clear                   wfnota            1
058100051129     c     kspe          chain     fnspe03l
058200051129      *
058300051129    1c                   if        %found(fnspe03l)
058400051129     c                   movel     'I'           wfluo
058500051129      * verifico se c'è indirizzo mail
058600051129     c     ksp2          chain     fnsp201l
058700051129    2c                   if        %found(fnsp201l) and SP2EST <> *BLANKS
058800051129     c                   movel     'M'           wfluo
058900051129    2c                   endif
059000051129    1c                   endif
059100051129
059200051129     c                   eval      Knk1 = '0151' + %editc(wksc:'X')
059300051129     c     Kntc          chain     Tfntc01l
059400051129    1c                   If        %found(tfntc01l) and ntcrnt <> *blanks
059500051129     c                   eval      wfnota = 'S'
059600051129    1c                   endif
059700051129     c                   endsr
059800060929      *---------------------------------------------------------------*
059900060929     C* Scrittura file di work per stampa ordinata per comm.le
060000060929      *---------------------------------------------------------------*
060100060929     C     wrt_wf        BEGSR
060200060929     C                   clear                   DS_cnaco
060300060929     C                   clear                   DS_cnind
060400060929     C                   clear                   DS_cnclp
060500060929     C                   clear                   DS_fncls
060600060929     c                   eval      I69kcp = clpscf
060700060929     C                   CALL      'TIBS69R'
060800060929     C                   PARM                    tibs69DS
060900060929     C                   PARM                    DS_cnaco
061000060929     C                   PARM                    DS_cnind
061100060929     C                   PARM                    DS_cnclp
061200060929     C                   PARM                    DS_fncls
061300060929     c                   clear                   wfta9100
061400060929     c                   z-add     clpscf        wta91scf
061500061002     c* recupero il commerciale unificante
061600130905     C     f_CLPage      CHAIN     AZCMM000
061700130905     c                   if        %found(AZCMM01L)
061800130905     C                   z-add     CMMuni        wta91age
061900130905     c                   else
062000130905     c                   z-add     f_CLPage      wTA91age
062100130905     c                   endif
062200060929     c                   write     wfta9100
062300060929     c                   endsr
062400060929      *---------------------------------------------------------------*
062500060929     C* Operazioni a rotuttra di cod.comm.le
062600060929      *---------------------------------------------------------------*
062700060929     C     sr_rotage     BEGSR
062800060929     c                   clear                   pdesage
062900060929     c* reperimento decodifica comm.le da tab.01
063000130905     C     CLPage        CHAIN     AZCMM000                           91
063100130905     C     *IN91         IFEQ      *Off
063200130905     C                   MOVEL     CMMdes        Pdesage
063300060929     C                   ENDIF
063400060929     c                   endsr
063500041124      *---------------------------------------------------------------*
063600970224     C* OPERAZIONI INIZIALI
063700041124      *---------------------------------------------------------------*
063800970224     C     *INZSR        BEGSR
063900970224     C*
064000970224     C     *ENTRY        PLIST
064100970224     C                   PARM                    KPJBA
064200050516     C                   MOVEl     KPJBU         tnta90ds
064300970224     C*
064400041116     c     *dtaara       define    §azute        azuteds
064500041116     c     *dtaara       define    §datiute      ddatiute
064600041116     c                   in(E)     *dtaara
064700041116     c                   If        %error  or RSUT = *blanks
064800041116     c                   CLEAR                   tibs34ds
064900041116     c                   CALL      'TIBS34R'
065000041116     c                   PARM                    tibs34ds
065100041116     c                   in        *dtaara
065200041116     c                   EndIf
065300050516     c* carico schiera pog dei p.o. da elaborare
065400050516     c                   clear                   pog
065500050516     c                   if        v1cpop <> '999'
065600050516     c                   movel     v1cpop        pog(1)
065700050516     c                   else
065800050516     c                   Clear                   wabi
065900050516      * Verifica errori e autorità profilo
066000050516s   1c                   Select
066100050516      * se ho errori nei dati utente esco dal pgm
066200050516      * se non c'è l'abilitazione
066300050516      * --> se 1° livello, abilitazioni al terminal
066400050516      *     se 2° livello, abilitazioni al punto operativo
066500050516w   1c                   When      UteAut = *Blanks
066600050516if  2c                   If        DutLpo = '1'
066700050516     c                   Eval      wabi   = 'TP'
066800050516e   2c                   EndIf
066900050516if  2c                   If        DutLpo = '2'
067000050516     c                   Eval      wabi   = 'PO'
067100050516e   2c                   EndIf
067200050516if  2c                   If        DutLpo = 'S'
067300050516     c                   Eval      wabi   = 'AZ'
067400050516e   2c                   EndIf
067500050516      * carica le abilitazioni del profilo
067600050516x   1c                   Other
067700050516     c                   Movel     UteFaf        Dute01
067800050516     c                   If        §UteCli <> *Blanks
067900050516     c                   Eval      wabi = §UteCli
068000050516     c                   Else
068100050516     c                   Eval      wabi = UteAut
068200050516     c                   EndIf
068300050516e   1c                   EndSl
068400050516
068500050516      * Reperimento dei P.O. gestibili dall'utente
068600050516     c                   clear                   TRUL31DS
068700050516     c                   eval      I31abi = wabi
068800050516     c                   eval      I31cdi = DUTdis
068900050516     c                   eval      I31car = DUTare
069000050516     c                   eval      I31cpo = DUTpou
069100050516     c                   call      'TRUL31R'
069200050516     c                   parm                    KPJBA
069300050516     c                   parm                    TRUL31DS
069400050516if  1c                   if        O31pog > *zeros
069500050516     c                   movea     O31pog        pog
069600050516e   1c                   endif
069700050516     c                   endif
069800050516     c
069900041118     c*
070000970224     C                   TIME                    WTIME            14 0
070100970224     C                   MOVE      WTIME         WDATE             8 0
070200970311     C                   MOVEL     WTIME         UTIME             6 0
070300050513     c*
070400050513     c     kclp          klist
070500050513     c                   kfld                    clpkut
070600050513     c                   kfld                    clpkcc
070700050513     c                   kfld                    clpscf
070800050513     C*-----CHIAVE FILE     TABEL00F
070900050513     C     CHIAV4        KLIST
071000050513     C                   KFLD                    codut
071100050513     C                   KFLD                    TIPTAB            2
071200050513     C                   KFLD                    KEYTAB
071300050513     C*-----CHIAVE FILE     TABEL00F
071400050513     C     KTA§          KLIST
071500050513     C                   KFLD                    codut
071600050513     C                   KFLD                    TIPTAB
071700051129      ** ACCESSO ANAGRAFICHE CLIENTI
071800051129     C     KSPE          KLIST
071900051129     C                   KFLD                    kFLS
072000051129     C                   KFLD                    wksc
072100051129     C                   KFLD                    kcod
072200051129      * accesso al file luoghi con indirizzo mail
072300051129     c     Ksp2          Klist
072400051129     c                   kfld                    specli
072500051129     c                   kfld                    specod
072600051129     c                   kfld                    comtpe
072700051129      *  file note tfntc01l
072800051129     C     Kntc          KLIST
072900051129     C                   KFLD                    Kapl
073000051129     C                   KFLD                    Knk1
073100051129     C                   KFLD                    Knk2
073200051129     C                   KFLD                    Ktnt
073300050513     C***
073400050513     C* CARICO TABELLA    FF  - FREQUENZA FATTURA
073500050513     C***
073600050513     C                   MOVEL     'FF'          TIPTAB
073700050513     C                   Z-ADD     0             B                 2 0
073800050513     C     KTA§          CHAIN     TABEL                              71
073900050513     C     *IN71         DOWEQ     *OFF
074000050513     C     TBLFLG        IFEQ      ' '
074100050513     C                   ADD       1             B
074200050513     C                   MOVEL     TBLKEY        CFF(B)
074300050513     C                   MOVEL(p)  TBLUNI        dsff
074400051129     c                   Movel     §ffdes1       DFF(B)
074500050513     C                   ENDIF
074600050513     C     KTA§          READE     TABEL                                  71
074700050513     C                   ENDDO
074800061002     c                   if        v1cord='C'
074900061002     c                   open      wfta911l
075000061002     c                   endif
075100051130     c* titolo dello spool
075200051130     c                   select
075300051130     c                   when      d90fatt = 'S'
075400051130     c                   eval      ptitolo = titfatt
075500051130     c                   when      d90anom = 'S'
075600051130     c                   eval      ptitolo = titanoma
075700051130     c                   other
075800051130     c                   eval      ptitolo = titolo
075900051130     c                   endsl
076000051201     c                   if        v1cpop <> '999'
076100051201     c                   movel(p)  v1cpop        p1ccli
076200051201     c                   else
076300051201     c                   movel     'Tutti'       p1ccli
076400051201     c                   endif
076500051201     c
076600050513     c*
076700050513     c                   write     rcd001
076800060929     c                   if        v1cord='K'
076900050513     c                   write     rcd003
077000060929     c                   z-add     7             $righe
077100060929     c                   else
077200060929     c                   z-add     3             $righe
077300060929     c                   endif
077400970224     C                   ENDSR
