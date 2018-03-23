000100090918      *PARMS CLOSQLCSR(*ENDMOD)
000200090918      *PARMS DYNUSRPRF(*OWNER)
000300090918     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000400090918
000500090918     H* TNVRMAIL*-----------------------------------------------------*
000600090918     H*         - Elenco clienti per sottoconto intestazione fattura  *
000700090918     H*---------------------------------------------------------------*
000800090918     FCNCLP02l  IF   E           K DISK    prefix(F_)
000900090918     Ftabel00f  IF   E           K DISK
001000090918     Ffnspe03l  if   e           k disk
001100090918     Ffnsp201l  if   e           k disk
001200090918     Ftfntc01l  if   e           k disk
001300090918     FTNVRMAP   O    E             PRINTER oflind(*In99)
001400090918
001500090918     D CFF             S              1  0 DIM(15)
001600090918     D DFF             S             30    DIM(15)
001700090918     d pog             s              3    dim(250)
001800090918     d wabi            s                   like(UteAut)
001900090918     d dsff          e ds
002000090918     d dtft          e ds
002100090918     d ds13          e ds
002200090918     d ds01          e ds
002300090918     d tnta90ds        ds
002400090918     d  v1cpop                 1      3    inz('999')
002500090918     d  d90abl                 4      4
002600090918     d  d90fatt                5      5
002700090918     d  d90anom                6      6
002800090918     d* ordinamento K=cliente; C=commerciale
002900090918     d  v1cord                 7      7    inz('K')
003000090918     D KPJBA         E DS
003100090918     d Azuteds       e ds                  Extname(Azute00f)
003200090918     d dDatiute      e ds
003300090918     d Tibs34ds      e ds                  Inz
003400090918     d dute01        e ds
003500090918     d trul31ds      e ds
003600090918     D* DS PER TIBS02R - GESTIONE TNTBE00F
003700090918     D DSBS02        E DS                  EXTNAME(TIBS02DS)
003800090918     d cnclpxxDs     e ds                  ExtName(cnclp00f)
003900090918     d TIBS69DS      E DS                  INZ
004000090918     d DS_cnaco      E DS                  extname(CNACO00F)
004100090918     d DS_cnind      E DS                  extname(CNIND00F)
004200090918     d DS_cnclp      E DS                  extname(CNCLP00F)
004300090918     d                                     prefix(F_)
004400090918     d DS_fncls      E DS                  extname(FNCLS00F)
004500090918     d
004600090918     D WrkGetListaclp  S           4500
004700090918     D                                     VARYING
004800090918     D WrkclpFilIn     S           4000
004900090918     D                                     VARYING
005000090918     d i               S              5I 0
005100090918
005200090918     D codut           S              1  0 inz(1)
005300090918     D keytab          S                   like(tblkey)
005400090918     d w_clpscf        s                   Like(clpscf)
005500090918     d s_clpscf        s                   Like(clpscf)
005600090918     d wksc            s                   Like(acoksc)
005700090918     d sav_age         s                   Like(clpage)
005800090918     d $righe          s              2  0
005900090921     D W_ELAB          S              1
006000090921     d rcd_elabora     s              1
006100090918     D Kapl            s                   LIKE(NTCapl) inz('C')
006200090918     D Knk1            s                   LIKE(NTCnk1)
006300090918     D Knk2            s                   LIKE(NTCnk2)
006400090918     D Ktnt            s                   LIKE(NTCtnt) inz('84')
006500090918     D Kfls            s                   LIKE(spefls) inz('L')
006600090918     D Kcod            s                   LIKE(specod) inz('001')
006700090918     D comtpe          s                   like(sp2tpe) inz('EM')
006800090918     D TITfatt         C                   CONST('         ELENCO CLIENTI PER S-
006900090918     D                                     OTTOCONTO INTESTAZIONE FATTURA SENZA-
007000090918     D                                      INVIO FATTURA VIA E-MAIL         ')
007100090918     D TITanoma        C                   CONST('           ELENCO CLIENTI PER-
007200090918     D                                      SOTTOCONTO INTESTAZIONE FATTURA CON-
007300090918     D                                      ANOMALIE INVIO FATTURA           ')
007400090918     D TITolo          C                   CONST('E L E N C O   C L I E N T I  -
007500090918     D                                      P E R   S O T T O C O N T O   I N T-
007600090918     D                                      E S T A Z I O N E   F A T T U R A')
007700090918
007800090918      *---------------------------------------------------------------*
007900090918     c* Eseguo sql per estrarre da cnclp solo i dati che mi servono
008000090918     c                   exsr      sr_Sqlclp
008100090918     c* Lettura ed elaborazione di cnclp
008200090918     c                   if        d90fatt <> 'S' and d90anom <> 'S'
008300090918     c                   exsr      sr_Leggiclp
008400090918     c                   else
008500090918     c                   exsr      sr_Leggiclp1
008600090918     c                   endif
008700090918     c
008800090918     c                   seton                                        lr
008900090918     c                   write     fines
009000090918     c                   write     rcd007
009100090918     c*
009200090918     C                   eval      I69TLA  = 'C'
009300090918     C                   call      'TIBS69R'
009400090918     C                   parm                    TIBS69DS
009500090918     c**********************************************************************
009600090918     c     Sr_Sqlclp     begsr
009700090918     **?Imposto la parte fissa dell'istruzione.
009800090918     C                   EVAL      WrkGetListaCLP =
009900090918     C                             'SELECT * from cnclp00f'
010000090918     **?WHERE
010100090918     C                   EVAL      WrkGetListaCLP = WrkGetListaCLP +
010200090918     C                             ' WHERE clpscf > 0 '
010300090918     **?Selezione su codice utente e capoconto
010400090918     C                   EVAL      WrkGetListaCLP = WrkGetListaCLP
010500090918     C                             +
010600090918     C                             ' AND clpkut = ' + %triml(%editc(codut:'Z'))
010700090918     c                             +
010800090918     c                             ' and clpkcc = ' + %triml(%editc(dutkci:'Z'))
010900090918     C                   EVAL      WrkGetListaCLP = WrkGetListaCLP
011000090918     C                             + ' ORDER BY clpkut, clpkcc, clpscf '
011100090918     **?Solo lettura.
011200090918     C                   EVAL      WrkGetListaCLP = WrkGetListaCLP +
011300090918     C                             ' FOR READ ONLY'
011400090918     C/EXEC SQL
011500090918     C+ PREPARE ListaCLP FROM :WrkGetListaCLP
011600090918     C/END-EXEC
011700090918     C
011800090918     C/EXEC SQL
011900090918     C+ DECLARE ListaCLP CURSOR FOR ListaCLP
012000090918     C/END-EXEC
012100090918     C
012200090918     C/EXEC SQL
012300090918     C+ OPEN ListaCLP
012400090918     C/END-EXEC
012500090918     c                   endsr
012600090918     c**********************************************************************
012700090918     c     Sr_LeggiClp   begsr
012800090918     c                   clear                   w_clpscf
012900090918     c*
013000090918     C/EXEC SQL
013100090918     C+ Fetch listaclp into :cnclpxxds
013200090918     C/END-EXEC
013300090918    1c                   Dow       Sqlcod = 0
013400090918     c* a rottura di sottoconto intestazione fattura rileggo cnclp per
013500090918     c* stampare tutti i clienti appartenenti allo stesso gruppo
013600090918
013700090918    3c                   if        clpscf <> w_clpscf
013800090918     c* se richiesto ordinamento per sottoconto int.fattura stampo altriment
013900090918     c* se richiesto ordinamento per comm.le memorizzo in workfile
014000090918     c* sottoc.int.fatt./codice comm.le
014100090918     c                   if        v1cord='K'
014200090918     c                   exsr      sr_elabora
014300090921      * verifico se ho elaborato il sotto conto intestazione
014400090921     c                   if        rcd_elabora <> 'N'
014500090921     c                   eval      w_clpscf = clpscf
014600090921     c                   endif
014700090921      *
014800090918    4c                   endif
014900090918    3c                   endif
015000090918     C/EXEC SQL
015100090918     C+ Fetch listaclp into :cnclpxxds
015200090918     C/END-EXEC
015300090918
015400090918    1c                   EndDo
015500090918     c*
015600090918
015700090918     C/EXEC SQL
015800090918     C+ close listaclp
015900090918     C/END-EXEC
016000090918     c                   endsr
016100090918     c**********************************************************************
016200090918     c     Sr_leggiclp1  begsr
016300090918     c                   clear                   w_clpscf
016400090918     c                   clear                   s_clpscf
016500090918     c*
016600090918     C/EXEC SQL
016700090918     C+ Fetch listaclp into :cnclpxxds
016800090918     C/END-EXEC
016900090918    1c                   Dow       Sqlcod = 0
017000090918     c                   if        clpscf <> s_clpscf
017100090918     c                   clear                   wscf              1
017200090918     c                   z-add     clpscf        s_clpscf
017300090918     c                   endif
017400090918     c                   if        clpscf <> w_clpscf
017500090918     c                   clear                   wprt              1
017600090918    2c                   if        w_elab = ' '
017700090918     c* letto codice non bloccato verifico in base alle selezioni se
017800090918     c* cliente da stampare
017900090918     c* verifico se raggruppamento da stampare
018000090918     c                   z-add     acoksc        wksc
018100090918     c                   exsr      sr_spenota
018200090918    3c                   select
018300090918     c* RICHIESTA STAMPA CLIENTI NO E-MAIL
018400090918     c                   when      d90fatt = 'S'
018500090918    4c                   select
018600090918     c* se non tovato ne' luogo ne' nota verifico il sottoconto intestaz.
018700090918     c* fattura
018800090918     c                   when      wfnota = *blanks  and wfluo=*blanks
018900090918     c                   if        clpscf = wksc
019000090918     c                   eval      wprt  = 'S'
019100090918     c                   else
019200090918     c                   if        wscf <> 'M'
019300090918     c                   z-add     clpscf        wksc
019400090918     c                   exsr      sr_spenota
019500090918     c                   if        (wfnota = *blanks and wfluo <> 'M') or
019600090918     c                              wfluo = 'I'
019700090918     c                   eval      wprt = 'S'
019800090918     c                   else
019900090918     c* se per scf ho trovato indirizzo e-mail me lo memorizzo per
020000090918     c* eventuali altri codici aventi lo stesso scf
020100090918     c                   eval      wscf = 'M'
020200090918     c                   endif
020300090918     c                   endif
020400090918     c                   endif
020500090918     c* se trovato luogo senza e-mail  --> no mail
020600090918     c                   when      wfluo = 'I'
020700090918     c                   eval      wprt = 'S'
020800090918    4c                   endsl
020900090918     c
021000090918     c* RICHIESTA STAMPA CLIENTI CON ANOMALIE INVIO FATTURA
021100090918     c                   when      d90anom = 'S' and wfnota ='S' and
021200090918     c                             wfluo <> *blanks
021300090918     c                   eval      wprt = 'S'
021400090918    3c                   endsl
021500090918    2c                   endif
021600090918     c* RAGGRUPPAMENTO DA STAMPARE
021700090918     c                   if        wprt = 'S'
021800090918     c* se richiesto ordinamento per sottoconto int.fattura stampo altriment
021900090918     c* se richiesto ordinamento per comm.le memorizzo in workfile
022000090918     c* sottoc.int.fatt./codice comm.le
022100090918     c                   if        v1cord='K'
022200090918     c* stampo  tutto il gruppo
022300090918     c                   exsr      sr_elabora
022400090921      * verifico se ho elaborato il sotto conto intestazione
022500090921     c                   if        rcd_elabora = 'N'
022600090921     c                   iter
022700090921     c                   endif
022800090921      *
022900090918     c                   endif
023000090918     c                   move      clpscf        w_clpscf
023100090918     c                   endif
023200090918     c                   endif
023300090918     C/EXEC SQL
023400090918     C+ Fetch listaclp into :cnclpxxds
023500090918     C/END-EXEC
023600090918
023700090918    1c                   EndDo
023800090918     c*
023900090918
024000090918     C/EXEC SQL
024100090918     C+ close listaclp
024200090918     C/END-EXEC
024300090918     c                   endsr
024400090918     c*
024500090918     c**********************************************************************
024600090918     c     Sr_Elabora    begsr
024700090921      * pulisco il flag di elaborazione
024800090921     c                   clear                   rcd_elabora
024900090918     c* stampo dati del sottoconto intestazione fattura
025000090918     C                   clear                   DS_cnaco
025100090918     C                   clear                   DS_cnind
025200090918     C                   clear                   DS_cnclp
025300090918     C                   clear                   DS_fncls
025400090918     c                   eval      I69kac = clpscf
025500090918     c                   eval      I69kcp = clpscf
025600090918     c                   eval      I69kcs = clpscf
025700090918     C                   CALL      'TIBS69R'
025800090918     C                   PARM                    tibs69DS
025900090918     C                   PARM                    DS_cnaco
026000090918     C                   PARM                    DS_cnind
026100090918     C                   PARM                    DS_cnclp
026200090918     C                   PARM                    DS_fncls
026300090921      * VERIFICO SE AUTOFATTURA ALTRIMENTI ESCO
026400090921     C                   If        F_clptft <> 1
026500090921     c                   eval      rcd_elabora = 'N'
026600090921     c                   leavesr
026700090921     c                   endif
026800090921
026900090918     c                   eval      punirsc = acorag
027000090918     c                   z-add     acoksc        pkscuni
027100090918     c                   clear                   pblk
027200090918     c                   if        acoabl = '8' or acoabl = '*'
027300090918     c                   eval      pblk = '*'
027400090918     c                   endif
027500090918     c                   eval      pfunif = %subst(clsflo:2:1)
027600090918     c**                 z-add     f_clptft      ptft
027700090918     c* decod. tipo fattura
027800090918     c                   Clear                   ptft_d
027900090918     c                   clear                   dsbs02
028000090918     C                   MOVEL     'C'           t02mod
028100090918     C                   MOVEL     knsif         t02sif
028200090918     C                   MOVEL     'TFT'         t02cod
028300090918     C                   MOVEL(P)  f_clptft      t02ke1
028400090918      *
028500090918     C                   CALL      'TIBS02R'
028600090918     C                   PARM                    KPJBA
028700090918     C                   PARM                    dsbs02
028800090918      *
028900090918     C                   if        t02err = *BLANKS
029000090918     c                   movel     t02uni        dtft
029100090918     c                   movel     §tftdes1      ptft_d
029200090918     c                   endif
029300090918
029400090918     c****               z-add     f_clpfft      pfft
029500090918     c* decod. frequenza fattura
029600090918     c                   Clear                   pfft_d
029700090918     c                   Eval      b = 1
029800090918     c     f_clpfft      Lookup    cff(b)                                 30
029900090918     c                   If        *In30
030000090918     c                   Movel     dff(b)        pfft_d
030100090918     c                   EndIf
030200090918     c*
030300090918     c                   movel     f_clpfun      ptdf              1
030400090918     c                   clear                   PTDF_D
030500090918     c* decod. tipo data fattura
030600090918     C                   MOVEL     '13'          TIPTAB
030700090918     C                   MOVEL(P)  ptdf          KEYTAB
030800090918     C     CHIAV4        CHAIN     TABEL00F                           91
030900090918     C     *IN91         IFEQ      *ON
031000090918     C     *IN91         OREQ      *OFF
031100090918     C     TBLFLG        ANDNE     ' '
031200090918     C                   ELSE
031300090918     C                   MOVEL     TBLUNI        ds13
031400090918     C                   MOVEL     §13des1       PTDF_D
031500090918     C                   ENDIF
031600090918     c* presenza luogo 001
031700090918     c* presenza nota 84
031800090918     c                   z-add     acoksc        wksc
031900090918     c                   exsr      sr_spenota
032000090918     c                   move      wfluo         pfluo
032100090918     c                   move      wfnota        pfnota
032200090918     c* sottoconto intestazione fattura se diverso
032300090918     c                   clear                   wprt006           1
032400090918     c                   if        f_clpscf <> f_clpksc
032500090918     c                   z-add     f_clpscf      PKSCUNI_U
032600090918     c* memorizzo di stampare rcd006
032700090918     c                   eval      wprt006 = 'S'
032800090918     c                   endif
032900090918     c* Gestione rottura cod.comm.le:
033000090918     c* se ordinamento per cod.comm.le, a rottura stampo riga del comm.le
033100090918     c*
033200090918     c                   if        ($righe + 1) > 60
033300090918     c                   write     rcd007
033400090918     c                   write     rcd001
033500090918     c                   if        v1cord='K'
033600090918     c                   write     rcd003
033700090918     c                   z-add     7             $righe
033800090918     c                   endif
033900090918     c                   endif
034000090918     c                   write     rcd004
034100090918     c                   add       1             $righe
034200090918     c
034300090918     c* stampo dati dei clienti unificati al sottoconto intest.fattura
034400090918     c
034500090918     c                   clear                   wprtprima         1
034600090918     c     kclp          setll     cnclp02l
034700090918     c                   do        *hival
034800090918     c     kclp          reade     cnclp02l
034900090918     c                   if        %eof(cnclp02l)
035000090918     c                   leave
035100090918     c                   endif
035200090918     c* ignoro record se ksc = scf
035300090918     c                   if        F_clpscf = F_clpksc
035400090918     c                   iter
035500090918     c                   endif
035600090918     c*
035700090918     C                   clear                   DS_cnaco
035800090918     C                   clear                   DS_cnind
035900090918     C                   clear                   DS_fncls
036000090918     c                   eval      I69kac = f_clpksc
036100090918     c                   eval      I69kcs = f_clpksc
036200090918     C                   CALL      'TIBS69R'
036300090918     C                   PARM                    tibs69DS
036400090918     C                   PARM                    DS_cnaco
036500090918     C                   PARM                    DS_cnind
036600090918     C                   PARM                    ds_cnclp
036700090918     C                   PARM                    DS_fncls
036800090918     c* scarto se bloccato e richiesti solo i non bloccati
036900090918     c                   if        d90abl='E' and (acoabl='*' or acoabl='8')
037000090918     c                   iter
037100090918     c                   endif
037200090918     c                   eval      U_punirsc = acorag
037300090918     c                   z-add     acoksc        U_pkscuni
037400090918     c                   clear                   U_pblk
037500090918     c                   if        acoabl = '8' or acoabl = '*'
037600090918     c                   eval      U_pblk = '*'
037700090918     c                   endif
037800090918     c                   eval      U_pfunif = %subst(clsflo:2:1)
037900090918     c***                z-add     f_clptft      U_ptft
038000090918     c* decod. tipo fattura
038100090918     c                   Clear                   U_ptft_d
038200090918     c                   clear                   dsbs02
038300090918     C                   MOVEL     'C'           t02mod
038400090918     C                   MOVEL     knsif         t02sif
038500090918     C                   MOVEL     'TFT'         t02cod
038600090918     C                   MOVEL(P)  f_clptft      t02ke1
038700090918      *
038800090918     C                   CALL      'TIBS02R'
038900090918     C                   PARM                    KPJBA
039000090918     C                   PARM                    dsbs02
039100090918      *
039200090918     C                   if        t02err = *BLANKS
039300090918     c                   movel     t02uni        dtft
039400090918     c                   movel     §tftdes1      U_ptft_d
039500090918     c                   endif
039600090918
039700090918     c****               z-add     f_clpfft      U_pfft
039800090918     c* decod. frequenza fattura
039900090918     c                   Clear                   U_pfft_d
040000090918     c                   Eval      b = 1
040100090918     c     f_clpfft      Lookup    cff(b)                                 30
040200090918     c                   If        *In30
040300090918     c                   Movel     dff(b)        U_pfft_d
040400090918     c                   EndIf
040500090918     c*
040600090918     c                   movel     f_clpfun      U_ptdf            1
040700090918     c                   clear                   U_PTDF_D
040800090918     c* decod. tipo data fattura
040900090918     C                   MOVEL     '13'          TIPTAB
041000090918     C                   MOVEL(P)  U_ptdf        KEYTAB
041100090918     C     CHIAV4        CHAIN     TABEL00F                           91
041200090918     C     *IN91         IFEQ      *ON
041300090918     C     *IN91         OREQ      *OFF
041400090918     C     TBLFLG        ANDNE     ' '
041500090918     C                   ELSE
041600090918     C                   MOVEL     TBLUNI        ds13
041700090918     C                   MOVEL     §13des1       U_PTDF_D
041800090918     C                   ENDIF
041900090918     c* presenza luogo 001
042000090918     c* presenza nota 84
042100090918     c                   z-add     acoksc        wksc
042200090918     c                   exsr      sr_spenota
042300090918     c                   move      wfluo         U_pfluo
042400090918     c                   move      wfnota        U_pfnota
042500090918     c                   if        $righe > 60
042600090918     c                   write     rcd007
042700090918     c                   write     rcd001
042800090918     c                   if        v1cord='K'
042900090918     c                   write     rcd003
043000090918     C                   write     rcd004
043100090918     c                   z-add     8             $righe
043200090918     c                   else
043300090918     c                   write     rcdage
043400090918     c                   write     rcd003
043500090918     c                   write     rcd004
043600090918     c                   z-add     10            $righe
043700090918     c                   endif
043800090918     c                   endif
043900090918     c                   if        wprtprima = 'S' and  wprt006 = 'S'
044000090918     c                   eval      ppar = '('
044100090918     c                   eval      ppar1 = ')'
044200090918     c                   write     rcd006
044300090918     c                   clear                   wprt006
044400090918     c                   endif
044500090918     c                   write     rcd005
044600090918     c                   add       1             $righe
044700090918     c                   eval      wprtprima = 'S'
044800090918     c*
044900090918     c                   enddo
045000090918     c*
045100090918     c                   if        wprt006 = 'S'
045200090918     c                   eval      ppar = '('
045300090918     c                   eval      ppar1 = ')'
045400090918     c                   write     rcd006
045500090918     c                   write     rcdspace
045600090918     c                   add       1             $righe
045700090918     c                   endif
045800090918     c*
045900090918     c                   endsr
046000090918     c**********************************************************************
046100090918     c     Sr_spenota    begsr
046200090918     c                   clear                   wfluo             1
046300090918     c                   clear                   wfnota            1
046400090918     c     kspe          chain     fnspe03l
046500090918      *
046600090918    1c                   if        %found(fnspe03l)
046700090918     c                   movel     'I'           wfluo
046800090918      * verifico se c'è indirizzo mail
046900090918     c     ksp2          chain     fnsp201l
047000090918    2c                   if        %found(fnsp201l) and SP2EST <> *BLANKS
047100090918     c                   movel     'M'           wfluo
047200090918    2c                   endif
047300090918    1c                   endif
047400090918
047500090918     c                   eval      Knk1 = '0151' + %editc(wksc:'X')
047600090918     c     Kntc          chain     Tfntc01l
047700090918    1c                   If        %found(tfntc01l) and ntcrnt <> *blanks
047800090918     c                   eval      wfnota = 'S'
047900090918    1c                   endif
048000090918     c                   endsr
048100090918      *---------------------------------------------------------------*
048200090921     C* Operazioni a rotura di cod.comm.le
048300090918      *---------------------------------------------------------------*
048400090918     C     sr_rotage     BEGSR
048500090918     c                   clear                   pdesage
048600090918     c* reperimento decodifica comm.le da tab.01
048700090918     C                   MOVEL     '01'          TIPTAB
048800090918     C                   MOVEL(P)  clpage        KEYTAB
048900090918     C     CHIAV4        CHAIN     TABEL00F                           91
049000090918     C     *IN91         IFEQ      *ON
049100090918     C     *IN91         OREQ      *OFF
049200090918     C     TBLFLG        ANDNE     ' '
049300090918     C                   ELSE
049400090918     C                   MOVEL     TBLUNI        ds01
049500090918     C                   MOVEL     §01age        Pdesage
049600090918     C                   ENDIF
049700090918     c                   endsr
049800090918      *---------------------------------------------------------------*
049900090918     C* OPERAZIONI INIZIALI
050000090918      *---------------------------------------------------------------*
050100090918     C     *INZSR        BEGSR
050200090918     C*
050300090918     C     *ENTRY        PLIST
050400090918     C                   PARM                    KPJBA
050500090921     C****               MOVEl     KPJBU         tnta90ds
050600090918     C*
050700090918     c     *dtaara       define    §azute        azuteds
050800090918     c     *dtaara       define    §datiute      ddatiute
050900090918     c                   in(E)     *dtaara
051000090918     c                   If        %error  or RSUT = *blanks
051100090918     c                   CLEAR                   tibs34ds
051200090918     c                   CALL      'TIBS34R'
051300090918     c                   PARM                    tibs34ds
051400090918     c                   in        *dtaara
051500090918     c                   EndIf
051600090918     c
051700090918     c*
051800090918     C                   TIME                    WTIME            14 0
051900090918     C                   MOVE      WTIME         WDATE             8 0
052000090918     C                   MOVEL     WTIME         UTIME             6 0
052100090918     c*
052200090918     c     kclp          klist
052300090918     c                   kfld                    clpkut
052400090918     c                   kfld                    clpkcc
052500090918     c                   kfld                    clpscf
052600090918     C*-----CHIAVE FILE     TABEL00F
052700090918     C     CHIAV4        KLIST
052800090918     C                   KFLD                    codut
052900090918     C                   KFLD                    TIPTAB            2
053000090918     C                   KFLD                    KEYTAB
053100090918     C*-----CHIAVE FILE     TABEL00F
053200090918     C     KTA§          KLIST
053300090918     C                   KFLD                    codut
053400090918     C                   KFLD                    TIPTAB
053500090918      ** ACCESSO ANAGRAFICHE CLIENTI
053600090918     C     KSPE          KLIST
053700090918     C                   KFLD                    kFLS
053800090918     C                   KFLD                    wksc
053900090918     C                   KFLD                    kcod
054000090918      * accesso al file luoghi con indirizzo mail
054100090918     c     Ksp2          Klist
054200090918     c                   kfld                    specli
054300090918     c                   kfld                    specod
054400090918     c                   kfld                    comtpe
054500090918      *  file note tfntc01l
054600090918     C     Kntc          KLIST
054700090918     C                   KFLD                    Kapl
054800090918     C                   KFLD                    Knk1
054900090918     C                   KFLD                    Knk2
055000090918     C                   KFLD                    Ktnt
055100090918     C***
055200090918     C* CARICO TABELLA    FF  - FREQUENZA FATTURA
055300090918     C***
055400090918     C                   MOVEL     'FF'          TIPTAB
055500090918     C                   Z-ADD     0             B                 2 0
055600090918     C     KTA§          CHAIN     TABEL                              71
055700090918     C     *IN71         DOWEQ     *OFF
055800090918     C     TBLFLG        IFEQ      ' '
055900090918     C                   ADD       1             B
056000090918     C                   MOVEL     TBLKEY        CFF(B)
056100090918     C                   MOVEL(p)  TBLUNI        dsff
056200090918     c                   Movel     §ffdes1       DFF(B)
056300090918     C                   ENDIF
056400090918     C     KTA§          READE     TABEL                                  71
056500090918     C                   ENDDO
056600090918     c* titolo dello spool
056700090918     c                   select
056800090918     c                   when      d90fatt = 'S'
056900090918     c                   eval      ptitolo = titfatt
057000090918     c                   when      d90anom = 'S'
057100090918     c                   eval      ptitolo = titanoma
057200090918     c                   other
057300090918     c                   eval      ptitolo = titolo
057400090918     c                   endsl
057500090918     c                   if        v1cpop <> '999'
057600090918     c                   movel(p)  v1cpop        p1ccli
057700090918     c                   else
057800090918     c                   movel     'Tutti'       p1ccli
057900090918     c                   endif
058000090918     c
058100090918     c*
058200090918     c                   write     rcd001
058300090918     c                   if        v1cord='K'
058400090918     c                   write     rcd003
058500090918     c                   z-add     7             $righe
058600090918     c                   else
058700090918     c                   z-add     3             $righe
058800090918     c                   endif
058900090918     C                   ENDSR
