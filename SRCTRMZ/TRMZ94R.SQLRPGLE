000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000102070926     fazorg01l  if   e           k disk
000103070731     ftabel00f  if   e           k disk
000104081105     fwffdta0f  o    e             disk
000105070730     fqsysprt   o    f  132        printer oflind(*inof)
000106070731     d* Parametri x Controllo profilo utenti
000107070731     d TIBS34ds      e ds
000108070731     d AZUTEds       e ds                  extname(AZUTE00F)
000109070926     D X16GGSETDS    e ds
000110070731     d DDatiUte      e ds
000111081105     D trmz93ds      e ds                  inz
000112070730     D trmz61ds      e ds                  inz
000114070731     d dsxn          e ds
000119070730     d  tga            s                   like(d61tga)
000120070730     d  tgn            s                   like(d61tgn)
000121070730     d  dta            s                   like(d61dta)
000122070926     d  nominativo     s             15
000123070926     d  dtaa           s              8
000124070926     d  oraa           s              6
000125070926     d  ind            s             24
000126070926     d  loc            s             20
000127070926     d  prov           s              2
000128070926     d  div            s              3
000129070926     d  prt            s             11  3
000130081119     d  qta            s              6  2
000133070921     d  savdtai        s                   like(d61dta)
000134070921     d  savdtaf        s                   like(d61dta)
000138070730     d  dtaiso         s               d
000139070730     d  dataiso        s               d
000140070731     d  dataeur        s               d   datfmt(*eur)
000141070731     d  sdtada         s               d   datfmt(*eur)
000142070731     d  sdtaa          s               d   datfmt(*eur)
000143070921     d  datai          s               d   datfmt(*eur)
000144070921     d  dataf          s               d   datfmt(*eur)
000812070730     d kpjba         e ds
000813070921     c                   eval      savdtai = *hival
000815070730     c* controllo agip
003100031028     C/EXEC SQL
003200070926     C+ DECLARE A1 CURSOR FOR SELECT NOMINATIVO, DATA_00003, ORA_V00001,
003201081119     C+ INDIR00001, LOCAL00001, PROV_PV, IMPORTO, QUANTITA FROM agip01l
003202081119     C+ WHERE DATA_00003 between :D93dtada and :D93dtaa and FLAG00001
003204081119     C+ ='C' and FAI_DA_TE ='N' ORDER BY NOMINATIVO, DATA_00003
003600031028     C/END-EXEC
003700031106
003800031106     C/EXEC SQL
003900031106     C+ OPEN A1
004000031106     C/END-EXEC
004100031028
004200070731     C                   do        *hival
004300031028
004400031028     C/EXEC SQL
004500070926     C+ FETCH NEXT FROM A1 INTO :nominativo, :dta, :oraa, :ind, :loc,
004501081119     C+ :prov, :prt, :qta
004600031028     C/END-EXEC
004700031028
004800031028     C                   select
004801070926     c* fine file
004900031028     C                   when      sqlcod = 100
004901070921     c                   move      savdtai       dataiso
004902070920     c                   move      dataiso       dataeur
004903070921     c                   move      dataeur       datai
004904070921     c                   move      savdtaf       dataiso
004905070921     c                   move      dataiso       dataf
004906070926     c* stampo periodo agip
004907070920     c                   except    ultagip
005001070731     c                   leave
005002070926     c* dettaglio
005400031028     C                   when      sqlcod = 0
005401070926     c* imposto i campi comuni
005402081106     c                   movel     'N'           fdtaflg
005403081106     c                   movel     nominativo    tga
005404070926     c                   eval      tgn = %subst(nominativo: 3: 3)+' ' +
005405070926     c                             %subst(nominativo: 6: 2)
005406081105     c                   eval      FDTaluogo = ind+loc+prov
005407070926     c                   if        oraa <> ' '
005408081105     c                   movel     oraa          FDTaora
005409070926     c                   else
005410081105     c                   clear                   FDTaora
005411070926     c                   end
005412070921     c* salvo la data del primo e dell'ultimo movimento trovato in archivio
005413070921     c                   if        dta < savdtai
005414070921     c                   eval      savdtai=dta
005415070920     c                   end
005416070921     c                   if        dta > savdtaf
005417070921     c                   eval      savdtaf=dta
005418070921     c                   end
005500070926     c* controllo il rifornimento
005600070730     c                   exsr      srctr
005700031028     C                   other
005800031028     C                   exsr      sqlerror
005900031028     C                   endsl
006000031028     C*
006100031028     C                   enddo
006200031028
006300031028     C/EXEC SQL
006400031028     C+ CLOSE A1
006500031028     C/END-EXEC
006583070920     c                   except    fine
006584070731     c                   seton                                        lr
006600031028     C**********************************************************************
006700081105     c* scrive record wffdta0f
006800031028     C**********************************************************************
006900070730     c     srctr         begsr
007008070730     c* reperisco l'autista
007009081119     c                   clear                   trmz61ds
007011070730     c                   eval      d61tga = tga
007012070730     c                   eval      d61tgn = tgn
007015070730     c                   move      dta           d61dta
007016070730     c                   call      'TRMZ61R'
007017070730     c                   parm                    trmz61ds
007018070730     c* ritorno E= errore bloccante A=anomalia
007020070730     c                   if        d61err <>' '
007021070730     c                   seton                                        11
007054070802     c                   end
007056081106     c* scrive o stampa
007058070731     c                   exsr      stampa
012500031028     c                   endsr
012508070730     C**********************************************************************
012509070730     C     stampa        begsr
012510070730     C**********************************************************************
012511070730     c*
012512070730     c                   if        *inof
012513070921     C                   EXCEPT    TESTA
012515070730     c                   except    testaagi
012520070730     c                   setoff                                       of
012521081105     c                   end
012522070730     c*
012523070730     c                   move      dta           dataiso
012524070730     c                   move      dataiso       dataeur
012525070731     c                   eval      tblkey = d61cauo
012526070731     c                   clear                   dsxn
012527070731     c     ktab          chain     tabel00f
012528070731     c                   if        %found(tabel00f)
012529070731     c                   eval      dsxn = tbluni
012530070731     c                   end
012531081105     c                   clear                   FDTafil
012532081105     c                   clear                   FDTaarea
012533081105     c                   clear                   FDTadis
012534070926     c                   if        d61cdc <> ' '
012535070926     c                   movel     d61cdc        comfil            3 0
012536070926     c     comfil        chain     azorg01l
012537070926     c                   if        %found(azorg01l)
012538081105     c                   eval      FDTafil = comfil
012539081105     c                   movel(p)  orgfl3        FDTadis
012540081105     c                   movel(p)  orgcar        FDTaarea
012541070926     c                   end
012542070926     c                   end
012548070730     c   11              except    err02
012551081106     c                   exsr      wtrwf
012553081119     c                   setoff                                       11
012554070730     C*
012555070730     C                   ENDSR
018000020709     C**********************************************************************
018100070921     C     wtrwf         BEGSR
018200020709     C**********************************************************************
018301070921     C*
018303081105     c                   movel     tga           FDTatgN
018304081105     c                   move      tgn           FDTatgn
018307081105     c                   eval      FDTadtu = dta
018308081105     c                   eval      FDTacau = d61cauo
018309081105     c                   eval      FDTades = §xndes
018324081105     c                   eval      FDTadiv = 'EUR'
018325081105     c                   eval      FDTaprt = prt
018326081119     c                   eval      FDTaqta = qta
018327081105     c                   write     wfFDTa00
018328070921     C*
018329070921     C                   ENDSR
018330081105     C**********************************************************************
018331081105     C     sqlerror      begsr
018332081105     C**********************************************************************
018333081105     C*
018334081105     C                   call      'X66CHGJOB'
018335081105     C                   seton                                        lr
018336081105     C                   ENDSR
018337070921     C**********************************************************************
018338070921     C     *inzsr        BEGSR
018339070921     C**********************************************************************
018340070921     c     *entry        plist
018400070730     c                   parm                    kpjba
018401081105     c                   movel     kpjbu         trmz93ds
018402081105     c                   move      D93dtada      dtaiso
018403070731     c                   move      dtaiso        sdtada
018404081105     c                   move      D93dtaa       dtaiso
018405070731     c                   move      dtaiso        sdtaa
018406070731      * Reperisco dati job
018407070731     c                   exsr      DatiJob
019001070731     C     Ktab          KLIST                                                  *
019002070731     C                   KFLD                    tblkut                         *
019003070731     C                   KFLD                    tblcod                         *
019004070731     C                   KFLD                    tblkey                         *
019005070731     c                   eval      tblkut = 1
019006070731     c                   eval      tblcod = 'XN'
019007081105     c                   seton                                        of
019400020709     C                   ENDSR
019401070731      *---------------------------------------------------------------*
019402070731      * Reperimento Dati del job (Utente/Operativi)                   *
019403070731      *---------------------------------------------------------------*
019404070731     c     DatiJob       BEGSR
019405070731      *
019406070731     c     *dtaara       define    §azute        azuteds
019407070731     c     *dtaara       define    §datiute      ddatiute
019408070731      *
019409070731     c                   in(E)     *dtaara
019410070731     c                   IF        %ERROR or RSUT = *blanks
019411070731     c                   clear                   Tibs34Ds
019412070731     c                   call      'TIBS34R'
019413070731     c                   parm                    Tibs34Ds
019414070731     c                   in        *dtaara
019415070731     c                   ENDIF
019416070731      *
019417070731     c                   ENDSR
019500070921     OQSYSPRT   E            TESTA            02
019501070731     O                       RSUT                20
019600081105     O                                         + 10 'CONTROLLO UTILIZZO FAI DA -
019601081105     o                                              TE AUTO AZIENDALI DA'
019602070731     O                       sdtada            +  1
019603070731     o                                         +  1 'A'
019604070731     O                       sdtaa             +  1
019900081105     O                                          113 'TRMZ94R'
020000070730     O                       UDATE              127 '  /  /  '
020100070730     O                       PAGE1         Z    132
020101070921     O          e            TESTAagi    2  2
020102070921     O                                         +  0 'CONTROLLO RIFORNIMENTO CAR'
020103070921     o                                         +  0 'BURANTE:'
021000070730     O          e            err02       1
021001070731     O                                         +  0 'Anagrafica automezzo non'
021002070731     O                                         +  1 'reperita'
021003070731     o                       tga               +  1
021004070731     o                       tgn               +  1
021005070921     o                                         +  1 ' in data'
021006070921     o                       dataeur           +  1
021503070920     O          e            ultagip     1
021504070921     O                                         +  0 'Periodo rilevato per rifor'
021505070921     O                                         +  0 'nimenti:'
021506070921     o                       datai             +  1
021507070921     O                                         +  1 'al'
021508070921     o                       dataf             +  1
021600070802     O          e            fine        1
021700070802     O                                         + 50 '*** FINE STAMPA ***'
